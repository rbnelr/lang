#pragma once
#include "common.hpp"

#include <inttypes.h>

#pragma warning(push, 0)
#pragma warning (disable : 4244)

//// IR gen
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"

#include "llvm/IR/NoFolder.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"

//// Optimize and machine code generation
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
//#include "llvm/PassRegistry.h"

//// JIT linking and exectution
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/Target/TargetMachine.h"

#include "llvm/ExecutionEngine/JITSymbol.h"
//#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
//#include "llvm/ExecutionEngine/Orc/Core.h"
//#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
//#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
//#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
//#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h" // warning C4244: 'initializing': conversion from '_Ty' to '_Ty2', possible loss of data

#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"

////
#include "llvm-c/Disassembler.h"

#pragma warning(pop)

_FORCEINLINE llvm::StringRef SR (std::string_view sv) {
	return { sv.data(), sv.size() };
}

#include "errors.hpp"

[[noreturn]] inline void ThrowErr (llvm::Error&& err) {
	std::string str;
	llvm::raw_string_ostream OS(str);
	OS << err;
	throw RuntimeExcept{ std::move(str) };
}

_FORCEINLINE void ThrowOnErr (llvm::Error&& err) {
	if (err)
		ThrowErr(std::move(err));
}

template <typename T>
_FORCEINLINE T ThrowOnErr (llvm::Expected<T>&& E) {
	if (E) return std::move(*E);

	ThrowErr(E.takeError());
}
