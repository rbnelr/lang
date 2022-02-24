#include "llvm_pch.hpp"
#include "llvm_backend.hpp"
#include "llvm_sec_mem_manager.hpp"
#include "llvm_disasm.hpp"

#include "common.hpp"
#include "frontend/builtins.hpp"

using namespace llvm;
using namespace llvm::orc;

struct JIT {

	llvm::ExitOnError     ExitOnErr;

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

	class MyPlugin : public ObjectLinkingLayer::Plugin {
	public:
		// The modifyPassConfig callback gives us a chance to inspect the
		// MaterializationResponsibility and target triple for the object being
		// linked, then add any JITLink passes that we would like to run on the
		// link graph. A pass is just a function object that is callable as
		// Error(jitlink::LinkGraph&). In this case we will add two passes
		// defined as lambdas that call the printLinkerGraph method on our
		// plugin: One to run before the linker applies fixups and another to
		// run afterwards.
		void modifyPassConfig(MaterializationResponsibility& MR,
			jitlink::LinkGraph& LG,
			jitlink::PassConfiguration& Config) override {

			outs() << "MyPlugin -- Modifying pass config for " << LG.getName() << " ("
				<< LG.getTargetTriple().str() << "):\n";

			// Print sections, symbol names and addresses, and any edges for the
			// associated blocks at the 'PostPrune' phase of JITLink (after
			// dead-stripping, but before addresses are allocated in the target
			// address space. See llvm/docs/JITLink.rst).
			//
			// Experiment with adding the 'printGraph' pass at other points in the
			// pipeline. E.g. PrePrunePasses, PostAllocationPasses, and
			// PostFixupPasses.
			Config.PostPrunePasses.push_back(printGraph);
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
		static void printBlockContent(jitlink::Block& B) {
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

		static Error printGraph(jitlink::LinkGraph& G) {

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



			return Error::success();
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
		RT->remove();
	}

	void setup_jit () {
		ZoneScoped;

		auto TT = Triple("x86_64-pc-windows-msvc-elf");

		{
			JITTargetMachineBuilder JTMB(TT);
			JTMB.setCodeModel(CodeModel::Small);
			JTMB.setRelocationModel(Reloc::PIC_);

			// still get eh_frames
			//TargetOptions O = JTMB.getOptions();
			//O.ExceptionModel = ExceptionHandling::None;
			//JTMB.setOptions(O);

			TM = ExitOnErr(JTMB.createTargetMachine());
		}

		{
			auto MM = std::make_unique<jitlink::InProcessMemoryManager>();
			auto SSP = std::make_shared<SymbolStringPool>();
			auto PageSize = ExitOnErr(sys::Process::getPageSize());


			auto EPC = std::make_unique<SelfExecutorProcessControl>(std::move(SSP), TT, PageSize, std::move(MM));
			ES = std::make_unique<ExecutionSession>(std::move(EPC));
		}

		DL = std::make_unique<DataLayout>(TM->createDataLayout()); // need to copy DL here (we can't simplt assign since DL does not have a default ctor)
		Mangle = std::make_unique<MangleAndInterner>(*ES, *DL);

		LL = std::make_unique<ObjectLinkingLayer>(*ES, ES->getExecutorProcessControl().getMemMgr());
		//LL->addPlugin(std::make_unique<MyPlugin>());

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
		MainJD->define(absoluteSymbols(std::move(builtin_syms)));
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
		if (TM->addPassesToEmitMC(MCgen, Ctx, ObjStream)) {
			ExitOnErr(llvm::make_error<llvm::StringError>(
				"Target does not support MC emission",
				llvm::inconvertibleErrorCode())
			);
		}
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

		LL->add(RT, std::move(ObjBuffer));
	}

	void compile (llvm::Module* modl) {
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

		auto str = Mangle->operator()("main");
		auto lookup = ExitOnErr(ES->lookup({ MainJD }, str));
		main = (main_fp)lookup.getAddress();
	}

	void execute () {
		ZoneScoped;

		print_seperator("Execute JITed LLVM code:");

		if (main)
			main();
		else
			assert(false);
	}

	void jit_and_execute (llvm::Module* modl) {
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
