#include "llvm_pch.hpp"
#include "llvm_backend.hpp"
#include "llvm_disasm.hpp"

#include "common.hpp"
#include "frontend/builtins.hpp"

using namespace llvm;
using namespace llvm::orc;

struct JIT {

	Triple                         TT;

	std::unique_ptr<TargetMachine> TM;
	std::unique_ptr<DataLayout> DL;

	legacy::PassManager        mem2reg;
	legacy::PassManager        optimize;
	legacy::PassManager        MCgen;

	std::unique_ptr<ExecutionSession> ES;

	std::unique_ptr<MangleAndInterner> Mangle;

	std::unique_ptr<ObjectLinkingLayer> LL;

	JITDylib* MainJD;
	ResourceTrackerSP RT;

	LoadedSections sections;

	class MemoryManager : public jitlink::JITLinkMemoryManager {
	public:
		LoadedSections& sections;

		MemoryManager (LoadedSections& sections) : sections{ sections } {}

		using Protect = sys::Memory::ProtectionFlags;

		enum AllocType {
			ReadExec,
			ReadOnly,
			ReadWrite,
		};
		static constexpr MmapProtect ALLOC_TYPE_PROT[] = {
			MMAP_READ | MMAP_EXEC ,
			MMAP_READ             ,
			MMAP_READ | MMAP_WRITE,
		};
		static AllocType map_alloc_type (Protect prot) {
			switch (prot) {
				case Protect::MF_READ | Protect::MF_EXEC:  return ReadExec;
				case Protect::MF_READ:                     return ReadOnly;
				case Protect::MF_READ | Protect::MF_WRITE: return ReadWrite;
					INVALID_DEFAULT;
			}
		}

		struct AllocBlock {
			void* ptr;
			size_t size;
		};

		// Local class for allocation.
		class Alloc : public jitlink::JITLinkMemoryManager::Allocation {
		public:

			AllocBlock block = {}; // single allocation for all
			AllocBlock segments[3] = {}; // seperated into 3 types of protection flags

			MutableArrayRef<char> getWorkingMemory (Protect Seg) override {
				auto type = map_alloc_type(Seg);
				return { (char*)segments[type].ptr, segments[type].size };
			}

			JITTargetAddress getTargetMemory (Protect Seg) override {
				auto type = map_alloc_type(Seg);
				return pointerToJITTargetAddress(segments[type].ptr);
			}

			void finalizeAsync (FinalizeContinuation OnFinalize) override {
				OnFinalize(applyProtections());
			}

			Error deallocate () override {
				mmap_pages_free(block.ptr);
				return Error::success();
			}

			Error applyProtections () {
				for (int i = 0; i < 3; ++i) {
					auto prot = ALLOC_TYPE_PROT[i];

					mmap_pages_protect(segments[i].ptr, segments[i].size, prot);
					//return errorCodeToError(EC);

					if (prot & MMAP_EXEC)
						mmap_invalidate_instruction_cache(segments[i].ptr, segments[i].size);
				}
				return Error::success();
			}

		};

		Expected<std::unique_ptr<Allocation>> allocate (
			const jitlink::JITLinkDylib* JD,
			const SegmentsRequestMap& Request) override {

			// Compute the total number of pages to allocate.
			size_t total_size = 0;
			for (auto& KV : Request) {
				const auto& Seg = KV.second;

				if (Seg.getAlignment() > mmap_page_size.page_size)
					return make_error<StringError>("Cannot request higher than page alignment", inconvertibleErrorCode());

				total_size = alignTo(total_size, mmap_page_size.page_size);
				total_size += Seg.getContentSize() + Seg.getZeroFillSize();
			}

			Alloc alloc = {};

			// alloc pages and remember actual allocated size
			alloc.block.ptr = mmap_pages_alloc(total_size, &alloc.block.size, MMAP_READ | MMAP_WRITE);
			assert(alloc.block.ptr != nullptr);
			//	return make_error<StringError>("mmap_pages_alloc failed", 0);

			char* cur = (char*)alloc.block.ptr;

			// Allocate segment memory from the slab.
			for (auto& KV : Request) {
				auto type = map_alloc_type((sys::Memory::ProtectionFlags)KV.first);

				auto content_size = KV.second.getContentSize();
				auto zero_fill_size = KV.second.getZeroFillSize();
				size_t size = content_size + zero_fill_size;

				char* ptr = cur;
				size_t alloc_size = alignTo(content_size + zero_fill_size, mmap_page_size.page_size);

				assert(ptr + size <= (char*)alloc.block.ptr + alloc.block.size && "Mapping exceeds allocation");

				// Zero out the zero-fill memory.
				memset(ptr + content_size, 0, zero_fill_size);

				assert(alloc.segments[type].size == 0);
				alloc.segments[type] = { ptr, alloc_size };

				if (options.print_disasm)
					sections.segments.push_back({ ALLOC_TYPE_PROT[type], ptr, size, alloc_size });

				cur += alloc_size;
			}

			return std::unique_ptr<Allocation>(new Alloc(alloc));
		}
	};

	class LinkerPlugin : public ObjectLinkingLayer::Plugin {
	public:
		LoadedSections& sections;

		LinkerPlugin (LoadedSections& sections) : sections{ sections } {}

		void modifyPassConfig(MaterializationResponsibility& MR,
			jitlink::LinkGraph& LG, jitlink::PassConfiguration& Config) override {

			assert(options.print_disasm); // LinkerPlugin only needed when we actually want disasm

			Config.PrePrunePasses .push_back([=] (jitlink::LinkGraph& G) { return pre_prune (G); });
			Config.PostFixupPasses.push_back([=] (jitlink::LinkGraph& G) { return post_fixup(G); });
		}

		void notifyLoaded(MaterializationResponsibility& MR) override {
			//outs() << "Loading object defining " << MR.getSymbols() << "\n";
		}

		Error notifyEmitted(MaterializationResponsibility& MR) override {
			//outs() << "Emitted object defining " << MR.getSymbols() << "\n";
			return Error::success();
		}

		Error notifyFailed(MaterializationResponsibility& MR) override {
			return Error::success();
		}

		Error notifyRemovingResources(ResourceKey K) override {
			return Error::success();
		}

		void notifyTransferringResources(ResourceKey DstKey,
			ResourceKey SrcKey) override {
		}

	private:

		void printBlockContent(jitlink::Block& B) {
			constexpr JITTargetAddress LineWidth = 16;

			if (B.isZeroFill()) {
				outs() << "    " << formatv("{0:x16}", B.getAddress()) << ": "
					<< B.getSize() << " bytes of zero-fill.\n";
				return;
			}

			JITTargetAddress InitAddr = B.getAddress() & ~(LineWidth - 1);
			JITTargetAddress StartAddr = B.getAddress();
			JITTargetAddress EndAddr = B.getAddress() + B.getSize();
			auto* Data = reinterpret_cast<const uint8_t*>(B.getContent().data());

			for (JITTargetAddress CurAddr = InitAddr; CurAddr != EndAddr; ++CurAddr) {
				if (CurAddr % LineWidth == 0)
					outs() << "          " << formatv("{0:x16}", CurAddr) << ": ";
				if (CurAddr < StartAddr)
					outs() << "   ";
				else
					outs() << formatv("{0:x-2}", Data[CurAddr - StartAddr]) << " ";
				if (CurAddr % LineWidth == LineWidth - 1)
					outs() << "\n";
			}
			if (EndAddr % LineWidth != 0)
				outs() << "\n";
		}
		
		// Make all symbols live so that they do not get removed by the linker so we can get their final addresses after linking
		// so we can print disassembly
		// NOTE: The link graph data is freed after linking, so there is no worry that symbols take up any space permanently
		// TODO: ^Though I do not know 100% that this won't cause more data to be included in the resulting sections
		//       We _may_ want to make sure to only mark desired symbols as live, like functions
		Error pre_prune (jitlink::LinkGraph& G) {
			
			for (auto& S : G.sections()) {
				for (auto* Sym : S.symbols()) {
					Sym->setLive(true);
				}
			}
			
			//print_graph(G);

			return Error::success();
		}

		Error post_fixup (jitlink::LinkGraph& G) {
			for (auto& S : G.sections()) {

				// add a symbol for the STUBS section
				strview name = { S.getName().data(), S.getName().size() };
				if (name == "$__STUBS") {
					// $__STUBS section contains 1 block for each external linked function
					// Those blocks represent the function stub, ie. jmp <4 bytes to GOT entry>
					// the block contains 1 edge whose target is the GOT entry symbol
					// the GOT entry symbol is associated with 1 block of whic the target symbol is the actual external function symbol
					// ex:
					// GOT_Sym  = Stub_Sec->blocks[i]->edges[0]->TargetSymbol
					// func_Sym = GOT_Sym->block->edges[0]->TargetSymbol
					// func_Sym->Name = "printf"

					if (S.blocks_size() > 0) {
						auto* B = *S.blocks().begin();
						sections.symbols.push_back({ (void*)B->getAddress(), "_STUBS" });
					}

					for (auto* B : S.blocks()) {
						if (B->edges_size() < 1) continue;
						auto& GOT_Sym = B->edges().begin()->getTarget();
						
						if (GOT_Sym.getBlock().edges_size() < 1) continue;
						auto& func_Sym = GOT_Sym.getBlock().edges().begin()->getTarget();
						
						if (!func_Sym.hasName()) continue;

						auto sr = func_Sym.getName();
						sections.symbols.push_back({ (void*)B->getAddress(), std::string(sr.data(), sr.size()) });
					}
				}

				for (auto* Sym : S.symbols()) {
					if (Sym->hasName()) {
						auto sr = Sym->getName();
						// need std::string since, link graph symbols strings seem to be deallocated before I want to print the disasm
						sections.symbols.push_back({ (void*)Sym->getAddress(), std::string(sr.data(), sr.size()) });
					}
				}
			}

			//print_graph(G);

			return Error::success();
		}

		void print_graph (jitlink::LinkGraph& G) {
			DenseSet<jitlink::Block*> BlocksAlreadyVisited;
			
			outs() << "Graph \"" << G.getName() << "\"\n";
			// Loop over all sections...
			for (auto& S : G.sections()) {
				outs() << "  Section " << S.getName() << ":\n";
			
				// Loop over all symbols in the current section...
				for (auto* Sym : S.symbols()) {
			
					// Print the symbol's address.
					outs() << "    " << formatv("{0:x16}", Sym->getAddress()) << ": ";
			
					// Print the symbol's name, or "<anonymous symbol>" if it doesn't have
					// one.
					if (Sym->hasName())
						outs() << Sym->getName() << "\n";
					else
						outs() << "<anonymous symbol>\n";
			
					// Get the content block for this symbol.
					auto& B = Sym->getBlock();
			
					if (BlocksAlreadyVisited.count(&B)) {
						outs() << "      Block " << formatv("{0:x16}", B.getAddress())
							<< " already printed.\n";
						continue;
					} else
						outs() << "      Block " << formatv("{0:x16}", B.getAddress())
						<< ":\n";
			
					outs() << "        Content:\n";
					printBlockContent(B);
					BlocksAlreadyVisited.insert(&B);
			
					if (!llvm::empty(B.edges())) {
						outs() << "        Edges:\n";
						for (auto& E : B.edges()) {
							outs() << "          "
								<< formatv("{0:x16}", B.getAddress() + E.getOffset())
								<< ": kind = " << formatv("{0:d}", E.getKind())
								<< ", addend = " << formatv("{0:x}", E.getAddend())
								<< ", target = ";
							jitlink::Symbol& TargetSym = E.getTarget();
							if (TargetSym.hasName())
								outs() << TargetSym.getName() << "\n";
							else
								outs() << "<anonymous target>\n";
						}
					}
					outs() << "\n";
				}
			}
		}
	};

	JIT () {
		ZoneScoped;

		// TODO: I can't pass -debug to my own app and expect LLVM to set this flag can I?
		// so just set it manually to print stuff?
	#ifndef NDEBUG
		llvm::DebugFlag = 0;

		//const char* dbg_types[] = {
		//};
		//if (llvm::DebugFlag)
		//	llvm::setCurrentDebugTypes(dbg_types, ARRLEN(dbg_types));
	#endif

		InitializeNativeTarget();
		InitializeNativeTargetAsmPrinter();
		InitializeNativeTargetAsmParser();
		InitializeNativeTargetDisassembler();

		setup_jit();

		// Opt level (for TM->addPassesToEmitMC() ???)
		TM->setOptLevel(llvm::CodeGenOpt::Default);

		setup_mem2reg();
		setup_optimize();
		//setup_MCgen();

		register_builtins();
	}
	~JIT () {
		ThrowOnErr(RT->remove());
	}

	void setup_jit () {
		ZoneScoped;

		TT = Triple("x86_64-pc-windows-msvc-elf");

		{
			JITTargetMachineBuilder JTMB(TT);
			JTMB.setCodeModel(CodeModel::Small);
			JTMB.setRelocationModel(Reloc::PIC_);

			 // with nounwind on all my functions I don't get eh_frames anymore, but still set this anyway, can't hurt I guess
			JTMB.getOptions().ExceptionModel = ExceptionHandling::None;

			TM = ThrowOnErr(JTMB.createTargetMachine());
		}

		{
			auto MM = std::make_unique<MemoryManager>(sections);
			auto SSP = std::make_shared<SymbolStringPool>();

			auto EPC = std::make_unique<SelfExecutorProcessControl>(std::move(SSP), TT, mmap_page_size.page_size, std::move(MM));
			ES = std::make_unique<ExecutionSession>(std::move(EPC));
		}

		DL = std::make_unique<DataLayout>(TM->createDataLayout()); // need to copy DL here (we can't simply assign since DL does not have a default ctor)
		Mangle = std::make_unique<MangleAndInterner>(*ES, *DL);

		LL = std::make_unique<ObjectLinkingLayer>(*ES, ES->getExecutorProcessControl().getMemMgr());

		if (options.print_disasm)
			LL->addPlugin(std::make_unique<LinkerPlugin>(sections));

		MainJD = &ES->createBareJITDylib("<main>");

		RT = MainJD->getDefaultResourceTracker();
	}

	void register_builtins () {
		ZoneScoped;

		SymbolMap builtin_syms;
		for (auto& bi : BUILTIN_FUNCS) {
			auto name = Mangle->operator()(SR(bi->ident));
			auto symb = JITEvaluatedSymbol{
				(JITTargetAddress)bi->builtin_func_ptr,
				JITSymbolFlags::Absolute | JITSymbolFlags::Callable
			};
			builtin_syms.insert({ name, symb });
		}
		ThrowOnErr(MainJD->define(absoluteSymbols(std::move(builtin_syms))));
	}


	void setup_mem2reg () {
		ZoneScoped;

		// mem2reg pass for alloca'd local vars
		mem2reg.add(llvm::createPromoteMemoryToRegisterPass());
	}
	void setup_optimize () {
		ZoneScoped;

		optimize.add(llvm::createCFGSimplificationPass()); //Dead code elimination
		optimize.add(llvm::createSROAPass());
		optimize.add(llvm::createLoopSimplifyCFGPass());

		optimize.add(llvm::createSCCPPass()); // createConstantPropagationPass ? 

		optimize.add(llvm::createNewGVNPass());//Global value numbering
		optimize.add(llvm::createReassociatePass());
		//optimize.add(llvm::createPartiallyInlineLibCallsPass()); //Inline standard calls
		optimize.add(llvm::createDeadCodeEliminationPass());
		optimize.add(llvm::createCFGSimplificationPass()); //Cleanup

		// This tries to turn  printf("\n");  into putchar, but my printf != C's printf?  O_o
		//optimize.add(llvm::createInstructionCombiningPass());

		optimize.add(llvm::createFlattenCFGPass()); //Flatten the control flow graph.
	}

	// TODO: why is the result-stream part of the passes manager?
	// if this was not the case we could easily prepare the PassManager once,
	// and compile with them multiple times
	void setup_MCgen (llvm::raw_svector_ostream& ObjStream) {
		ZoneScoped;

		llvm::MCContext* Ctx;
		if (TM->addPassesToEmitMC(MCgen, Ctx, ObjStream))
			throw RuntimeExcept("Target does not support MC emission");
	}


	void run_mem2reg (llvm::Module* modl) {
		ZoneScoped;

		mem2reg.run(*modl);

		if (options.print_ir) {
			print_seperator("LLVM IR - After mem2reg");
			modl->print(llvm::errs(), nullptr);
		}
	}
	void run_optimize (llvm::Module* modl) {
		ZoneScoped;

		optimize.run(*modl);

		if (options.print_ir) {
			print_seperator("LLVM IR - After optimize");
			modl->print(llvm::errs(), nullptr);
		}
	}

	void run_MCgen (llvm::Module* modl) {
		ZoneScoped;

		// can't really do this upfront because of LLVM API weirdness
		// SmallVector gets stored in raw_svector_ostream (which is not just a downcasted ref to SmallVector btw)
		// then this has to be passed into addPassesToEmitMC
		// later PassManager can be run, only after which MCgen_ObjBufferSV and ObjStream can go out of scope
		// furthermore SmallVector then has to be _moved_ into SmallVectorMemoryBuffer to call addObjectFile
		// what a mess of ownership and lifetimes for no apparent reason...
		// > PassManager should not own its output buffer, especially not during setup, why not just pass it in when run?
		//   and why should using a jitted buffer require taking ownership!? (it's just memory, right? why steal and then destroy it?)
		llvm::SmallVector<char, 0> MCgen_ObjBufferSV;
		llvm::raw_svector_ostream ObjStream(MCgen_ObjBufferSV);

		setup_MCgen(ObjStream);

		MCgen.run(*modl);

		auto ObjBuffer = std::make_unique<llvm::SmallVectorMemoryBuffer>(
			std::move(MCgen_ObjBufferSV),
			modl->getModuleIdentifier() + "-jitted-objectbuffer"
		);

	#ifndef NDEBUG
		llvm::DebugFlag = 0;
	#endif

		ThrowOnErr(LL->add(RT, std::move(ObjBuffer)));
	}

	void compile (llvm::Module * modl) {
		ZoneScoped;

		modl->setDataLayout(*DL);

		run_mem2reg(modl);

		if (options.optimized)
			run_optimize(modl);

		run_MCgen(modl);
	}

	typedef void (*main_fp)();
	main_fp main = nullptr;

	void load () {
		ZoneScoped;

		// linking actually happens during lookup (and the original ObjBuffer, LinkGraph etc. are deleted)
		auto str = Mangle->operator()("main");
		auto lookup = ThrowOnErr(ES->lookup({ MainJD }, str));
		main = (main_fp)lookup.getAddress();

		if (options.print_disasm)
			print_llvm_disasm(TT, sections);
	}

	void execute () {
		ZoneScoped;

		print_seperator("Execute JITed LLVM code:");

		if (main)
			main();
		else
			assert(false);
	}

	void jit_and_execute (llvm::Module * modl) {
		ZoneScoped;

		compile(modl);
		load();
		execute();
	}
};

void llvm_jit_and_exec (llvmModule& modl) {
	ZoneScoped;

	JIT jit{};
	jit.jit_and_execute(modl.modl);
}
