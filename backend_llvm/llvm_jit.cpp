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

	std::unique_ptr<LLJIT>         jit;
	
	legacy::PassManager        mem2reg;
	legacy::PassManager        optimize;
	legacy::PassManager        MCgen;

	
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

	void setup_jit () {
		ZoneScoped;

		//auto triple_str = llvm::sys::getProcessTriple();
		std::string triple_str = "x86_64-pc-windows-msvc-elf";
		auto TT = Triple(triple_str);
		
		JITTargetMachineBuilder JTMB(TT);
		JTMB.setCodeModel(CodeModel::Small);
		JTMB.setRelocationModel(Reloc::PIC_);

		TM = ExitOnErr(JTMB.createTargetMachine());

		LLJITBuilder JB;
		JB.setJITTargetMachineBuilder(std::move(JTMB));
		JB.setObjectLinkingLayerCreator([&](ExecutionSession& ES, const Triple& TT) {
			return std::make_unique<ObjectLinkingLayer>(ES, std::make_unique<jitlink::InProcessMemoryManager>());
		});
		jit = ExitOnErr(JB.create());
	}

	void register_builtins () {
		ZoneScoped;

		SymbolMap builtin_syms;
		for (auto& bi : BUILTIN_FUNCS) {
			auto name = jit->mangleAndIntern(SR(bi->ident));
			auto symb  = JITEvaluatedSymbol{
				(JITTargetAddress)bi->builtin_func_ptr,
				JITSymbolFlags::Absolute | JITSymbolFlags::Callable
			};
			builtin_syms.insert({ name, symb });
		}
		jit->getMainJITDylib().define(absoluteSymbols(std::move(builtin_syms)));
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
			ExitOnErr( llvm::make_error<llvm::StringError>(
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
		// SmallVector gets stored in raw_svector_ostream (which is not just a downcasted ref to SmallVector)
		// then this has to be passed into addPassesToEmitMC
		// later PassManager can be run, only after which MCgen_ObjBufferSV and ObjStream can go out of scope
		// furthermore SmallVector then has to be _moved_ into SmallVectorMemoryBuffer to call addObjectFile
		// what a mess of ownership and lifetimes for no apparent reason...
		// > PassManager should not own its output buffer, especially not during setup, why not just pass it in when run?
		//   and why should using a jitted buffer require taking ownership!? (and then destroying it)
		llvm::SmallVector<char, 0> MCgen_ObjBufferSV;
		llvm::raw_svector_ostream ObjStream( MCgen_ObjBufferSV );

		setup_MCgen(ObjStream);

		MCgen.run(*modl);

		auto ObjBuffer = std::make_unique<llvm::SmallVectorMemoryBuffer>(
			std::move(MCgen_ObjBufferSV),
			modl->getModuleIdentifier() + "-jitted-objectbuffer"
		);
		
		ExitOnErr(jit->addObjectFile(std::move(ObjBuffer)));

		//if (options.print_code) {
		//	print_llvm_disasm(TT, dyld, *Obj, *loadedObj, MM);
		//}
	}

	void compile_and_load (llvm::Module* modl) {
		ZoneScoped;
		
		auto DL = TM->createDataLayout();

		modl->setDataLayout(DL);
		
		run_mem2reg(modl);

		if (options.optimized)
			run_optimize(modl);

		run_MCgen(modl);
	}
	void execute () {
		ZoneScoped;

		print_seperator("Execute JITed LLVM code:");

		typedef void (*main_fp)();
		auto fptr = (main_fp)ExitOnErr(jit->lookup("main")).getAddress();
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

void llvm_jit_and_exec (llvmModule& modl) {
	ZoneScoped;

	JIT jit {};
	jit.jit_and_execute(modl.modl);
}
