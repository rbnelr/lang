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

		auto TT = Triple("x86_64-pc-windows-msvc-elf");
		
		{
			JITTargetMachineBuilder JTMB(TT);
			JTMB.setCodeModel(CodeModel::Small);
			JTMB.setRelocationModel(Reloc::PIC_);

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

		MainJD = &ES->createBareJITDylib("<main>");

		RT = MainJD->getDefaultResourceTracker();
	}

	void register_builtins () {
		ZoneScoped;
		
		SymbolMap builtin_syms;
		for (auto& bi : BUILTIN_FUNCS) {
			auto name = Mangle->operator()(SR(bi->ident));
			auto symb  = JITEvaluatedSymbol{
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
		// SmallVector gets stored in raw_svector_ostream (which is not just a downcasted ref to SmallVector btw)
		// then this has to be passed into addPassesToEmitMC
		// later PassManager can be run, only after which MCgen_ObjBufferSV and ObjStream can go out of scope
		// furthermore SmallVector then has to be _moved_ into SmallVectorMemoryBuffer to call addObjectFile
		// what a mess of ownership and lifetimes for no apparent reason...
		// > PassManager should not own its output buffer, especially not during setup, why not just pass it in when run?
		//   and why should using a jitted buffer require taking ownership!? (it's just memory, right? why steal and then destroy it?)
		llvm::SmallVector<char, 0> MCgen_ObjBufferSV;
		llvm::raw_svector_ostream ObjStream( MCgen_ObjBufferSV );

		setup_MCgen(ObjStream);

		MCgen.run(*modl);

		auto ObjBuffer = std::make_unique<llvm::SmallVectorMemoryBuffer>(
			std::move(MCgen_ObjBufferSV),
			modl->getModuleIdentifier() + "-jitted-objectbuffer"
		);

	#ifndef NDEBUG
		llvm::DebugFlag = 1;
	#endif
		
		LL->add(RT, std::move(ObjBuffer));
	}

	void compile_and_load (llvm::Module* modl) {
		ZoneScoped;
		
		modl->setDataLayout(*DL);
		
		run_mem2reg(modl);

		if (options.optimized)
			run_optimize(modl);

		run_MCgen(modl);
	}
	void execute () {
		ZoneScoped;

		print_seperator("Execute JITed LLVM code:");

		typedef void (*main_fp)();

		auto str = Mangle->operator()("main");
		auto lookup = ExitOnErr(ES->lookup({ MainJD }, str));
		auto fptr = (main_fp)lookup.getAddress();

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
