
#include <inttypes.h>

#pragma warning(push, 0)
#pragma warning (disable : 4244)

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h" // warning C4244: 'initializing': conversion from '_Ty' to '_Ty2', possible loss of data

#pragma warning(pop)

//int main () {
//	llvm_test();
//	return 0;
//}

using namespace llvm;
using namespace llvm::orc;

ExitOnError ExitOnErr;

struct JIT {
	
	ThreadSafeContext TSC;
	ThreadSafeModule  TSM;

	std::unique_ptr<ExecutionSession>  ES;

	std::unique_ptr<DataLayout>               DL;
	std::unique_ptr<MangleAndInterner>        Mangle;
	
	std::unique_ptr<RTDyldObjectLinkingLayer> ObjectLayer;
	std::unique_ptr<IRCompileLayer>           CompileLayer;
	
	JITDylib*                                 MainJD;

	void create () {
		// TODO: I can't pass -debug to my own app and expect LLVM to set this flag can I?
		// so just set it manually to print stuff?
	#ifndef NDEBUG
		//llvm::DebugFlag = true;
	#endif

		InitializeNativeTarget();
		InitializeNativeTargetAsmPrinter();
		InitializeNativeTargetAsmParser();
		
		TSC = ThreadSafeContext(std::make_unique<LLVMContext>());
		TSM = ThreadSafeModule(std::make_unique<Module>("llvm_test", *TSC.getContext()), TSC);

		auto EPC = ExitOnErr( SelfExecutorProcessControl::Create() );
		ES = std::make_unique<ExecutionSession>(std::move(EPC));

		JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());

		DL = std::make_unique<DataLayout>(ExitOnErr( JTMB.getDefaultDataLayoutForTarget() ));

		Mangle = std::make_unique<MangleAndInterner>(*ES, *DL);

		ObjectLayer = std::make_unique<RTDyldObjectLinkingLayer>(*this->ES, []() { return std::make_unique<SectionMemoryManager>(); });

		CompileLayer = std::make_unique<IRCompileLayer>(*this->ES, *ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB)));

		MainJD = &ES->createBareJITDylib("<main>");

		MainJD->addGenerator(cantFail(
			DynamicLibrarySearchGenerator::GetForCurrentProcess(DL->getGlobalPrefix())
		));

		TSM.getModuleUnlocked()->setDataLayout(*DL);

		if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
			ObjectLayer->setOverrideObjectFlagsWithResponsibilityFlags(true);
			ObjectLayer->setAutoClaimResponsibilityForObjectSymbols(true);
		}
	}

	void destroy () {
		if (auto Err = ES->endSession())
			ES->reportError(std::move(Err));
	}

	typedef float (*func_t)(float arg0, float arg1);

	static Function* compile_test (LLVMContext* ctx, Module* modl) {

		auto Builder = IRBuilder<>(*ctx);

		std::vector<Type*> args = {
			Type::getFloatTy(*ctx),
			Type::getFloatTy(*ctx),
		};

		auto* FT = FunctionType::get(Type::getFloatTy(*ctx), args, false);

		auto* F = Function::Create(FT, Function::ExternalLinkage, "test", *modl);

		auto arg = F->args().begin();
		auto& arg0 = *arg++;
		auto& arg1 = *arg++;

		arg0.setName("arg0");
		arg1.setName("arg1");

		auto* BB = BasicBlock::Create(*ctx, "entry", F);
		Builder.SetInsertPoint(BB);

		//
		auto* a = &arg0;
		auto* b = ConstantFP::get(*ctx, APFloat(2.0f));

		auto* c = Builder.CreateFAdd(a, b, "addtmp");

		auto* d = &arg1;
		auto* e = Builder.CreateFMul(c, d, "multmp");

		// Finish off the function.
		Builder.CreateRet(e);

		verifyFunction(*F);

		modl->print(errs(), nullptr);

		return F;
	}

	void run_test () {
		auto RT = MainJD->getDefaultResourceTracker();
		CompileLayer->add(RT, std::move(TSM));

		auto func = ES->lookup({MainJD}, (*Mangle)("test"));
		auto faddr = func->getAddress();

		auto fptr = (func_t)faddr;

		float a0 = 5.0f, a1 = 0.5f;
		float res = fptr(a0, a1);

		printf("test(%f, %f) = %f\n", a0, a1, res);
	}
};

void llvm_test () {
	JIT jit;
	jit.create();
	jit.compile_test(jit.TSC.getContext(), jit.TSM.getModuleUnlocked());
	jit.run_test();
	jit.destroy();

	exit(0);
}
