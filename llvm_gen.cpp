
#include <inttypes.h>

#pragma warning(push, 0)

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "KaleidoscopeJIT.h"

#pragma warning(pop)


int main () {
	using namespace llvm;

	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	auto TheContext = std::make_unique<LLVMContext>();
	auto Builder = IRBuilder<>(*TheContext);

	auto TheModule = std::make_unique<Module>("llvm_test", *TheContext);

	auto TSC = llvm::orc::ThreadSafeContext(std::move(TheContext));

	llvm::orc::ThreadSafeModule TSM(std::move(TheModule), TSC);

	auto TheJIT = llvm::orc::KaleidoscopeJIT::Create();
	auto jit = TheJIT->get();
	
	auto ctx = TSM.getContext().getContext();
	auto modl = TSM.getModuleUnlocked();
	modl->setDataLayout(jit->getDataLayout());

	
	
	//
	typedef float (*func_t)(float arg0, float arg1);

	std::vector<llvm::Type*> args = {
		llvm::Type::getFloatTy(*ctx),
		llvm::Type::getFloatTy(*ctx),
	};

	FunctionType *FT = FunctionType::get(llvm::Type::getFloatTy(*ctx), args, false);

	Function *F = Function::Create(FT, Function::ExternalLinkage, "test", *modl);

	auto arg = F->args().begin();
	auto& arg0 = *arg++;
	auto& arg1 = *arg++;
	
	arg0.setName("arg0");
	arg1.setName("arg1");

	BasicBlock *BB = BasicBlock::Create(*ctx, "entry", F);
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
	
	
	auto err = jit->addModule(std::move(TSM));

	auto func = jit->lookup("test");
	auto faddr = func->getAddress();
	
	auto fptr = (func_t)faddr;

	float a0 = 5.0f, a1 = 0.5f;
	float res = fptr(a0, a1);

	printf("test(%f, %f) = %f\n", a0, a1, res);
	return 0;
}

