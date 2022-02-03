#include "llvm_pch.hpp"
#include "llvm_backend.hpp"
#include "llvm_sec_mem_manager.hpp"
#include "llvm_disasm.hpp"

#include "common.hpp"
#include "builtins.hpp"

struct JIT {

//// Resolver (why do I need this? possibly once I combine multiple modules?)
	class Resolver : public llvm::JITSymbolResolver {
	public:

		// TODO: is there a way to simply declare my builtins as symbols so this entire Resolver class would become unnesassary?
		//       or is this generally needed to link multiple modules together?
		std::map<llvm::StringRef, llvm::JITTargetAddress> external_funcs;

		Resolver () {}
		virtual ~Resolver() = default;
		
		// this seems to be called for function symbols that are not in the code
		// which includes my builtin functions
		// TODO: is there a way to simply declare my builtins as external (to be linked) functions?

		/// Returns the fully resolved address and flags for each of the given
		///        symbols.
		///
		/// This method will return an error if any of the given symbols can not be
		/// resolved, or if the resolution process itself triggers an error.
		virtual void lookup(const LookupSet &Symbols,
							OnResolvedFunction OnResolved) {

			std::map<llvm::StringRef, llvm::JITEvaluatedSymbol> results;

			for (auto& Sym : Symbols) {
				auto it = external_funcs.find(Sym);
				if (it != external_funcs.end()) {
					results.emplace(Sym, llvm::JITEvaluatedSymbol{
						it->second,
						llvm::JITSymbolFlags::Absolute | // TODO: do I need this?
						llvm::JITSymbolFlags::Callable
					});
				}
				else {
					assert(false);
				}
			}

			OnResolved(results);
		}

		// No idea what this is supposed to do, I always seem to be called with an empty LookupSet

		/// Returns the subset of the given symbols that should be materialized by
		/// the caller. Only weak/common symbols should be looked up, as strong
		/// definitions are implicitly always part of the caller's responsibility.
		virtual llvm::Expected<LookupSet>
		getResponsibilitySet(const LookupSet &Symbols) {
			LookupSet Result;
			assert(Symbols.size() == 0);
			return Result;
		}

		/// Specify if this resolver can return valid symbols with zero value.
		//virtual bool allowsZeroSymbols() { return false; }

	};
	void register_builtins () {
		for (auto& builtin : builtin_funcs) {
			resolver.external_funcs.emplace(SR(builtin->ident), (llvm::JITTargetAddress)builtin->builtin_func_ptr);
		}
	}

////
	Resolver                             resolver;
	SectionMemoryManager                 MM;
	
	llvm::RuntimeDyld                    dyld;

	llvm::Triple                         TT;

	std::unique_ptr<llvm::TargetMachine> TM;

	
	llvm::legacy::PassManager mem2reg;
	llvm::legacy::PassManager optimize;
	llvm::legacy::PassManager MCgen;
	
	JIT (): resolver{}, MM{}, dyld{MM, resolver} {
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

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();
		llvm::InitializeNativeTargetDisassembler();

		register_builtins();

		auto triple_str = llvm::sys::getProcessTriple();
		TT = llvm::Triple(triple_str);

		llvm::orc::JITTargetMachineBuilder JTMB(TT);

		TM = llvm::cantFail( JTMB.createTargetMachine() );

	////
		// Opt level (for TM->addPassesToEmitMC() ???)
		TM->setOptLevel(llvm::CodeGenOpt::Default);

		setup_mem2reg();
		setup_optimize();
		//setup_MCgen();
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

	// TODO: why is the result stream part of the passes manager?
	// if this was not the case we could easily prepare the PassManager once,
	// and compile with them multiple times
	void setup_MCgen (llvm::raw_svector_ostream& ObjStream) {
		ZoneScoped;

		llvm::MCContext* Ctx;
		if (TM->addPassesToEmitMC(MCgen, Ctx, ObjStream)) {
			ExitOnErr( llvm::make_error<llvm::StringError>(
				"Target does not support MC emission",
				llvm::inconvertibleErrorCode())
			);
		}
	}
		
	void run_mem2reg (llvm::Module* modl) {
		mem2reg.run(*modl);

		if (options.print_ir) {
			print_seperator("LLVM IR - After mem2reg");
			modl->print(llvm::errs(), nullptr);
		}
	}
	void run_optimize (llvm::Module* modl) {
		optimize.run(*modl);
		
		if (options.print_ir) {
			print_seperator("LLVM IR - After optimize");
			modl->print(llvm::errs(), nullptr);
		}
	}

	void run_MCgen (llvm::Module* modl) {

		llvm::SmallVector<char, 0> ObjBufferSV;
		llvm::raw_svector_ostream ObjStream(ObjBufferSV);

		setup_MCgen(ObjStream);
		
		MCgen.run(*modl);

		llvm::SmallVectorMemoryBuffer ObjBuffer {
			std::move(ObjBufferSV),
			modl->getModuleIdentifier() + "-jitted-objectbuffer"
		};
			
		auto Obj = ExitOnErr(
			llvm::object::ObjectFile::createObjectFile(ObjBuffer.getMemBufferRef())
		);
		
		std::unique_ptr<llvm::RuntimeDyld::LoadedObjectInfo> loadedObj;
		{
			ZoneScopedN("dyld.loadObject()");
			loadedObj = dyld.loadObject(*Obj);
		}
		
		{
			ZoneScopedN("dyld.finalizeWithMemoryManagerLocking()");
			dyld.finalizeWithMemoryManagerLocking(); // calls resolveRelocations
		}

		if (options.print_code) {
			DisasmPrinter disasm(TT);
			disasm.print_disasm(dyld, *Obj, *loadedObj, MM);
		}
	}

	void compile_and_load (llvm::Module* modl) {
		ZoneScoped;
		
		auto DL = TM->createDataLayout();

		modl->setDataLayout(DL);
		
		run_mem2reg(modl);
		run_optimize(modl);
		run_MCgen(modl);
	}
	void execute () {
		ZoneScoped;

		print_seperator("Execute JITed LLVM code:");

		typedef void (*main_fp)();
		auto fptr = (main_fp)dyld.getSymbol("main").getAddress();
		if (fptr)
			fptr();
		else
			assert(false);
	}
	
	void jit_and_execute (llvm::Module* modl) {
		ZoneScoped;

		compile_and_load(modl);
		execute();
	}
};

void llvm_jit_and_exec (llvm::Module* modl) {
	ZoneScoped;

	JIT jit {};
	jit.jit_and_execute(modl);
}
