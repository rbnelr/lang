#pragma once
#include "backend_llvm/llvm_pch.hpp"
#include "common.hpp"
#include "frontend/builtins.hpp"

#pragma warning(push, 0)
#pragma warning (disable : 4244)

#include "llvm/ADT/StringMap.h"
#include "llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/SourceMgr.h"

#include "llvm/ExecutionEngine/JITLink/JITLink.h"

#pragma warning(pop)

llvm::ExitOnError ExitOnErr;

using namespace llvm;
using namespace llvm::orc;

void llmv_test (std::unique_ptr<llvm::Module> modl, std::unique_ptr<llvm::LLVMContext> ctx) {
	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();
	llvm::InitializeNativeTargetAsmParser();
	llvm::InitializeNativeTargetDisassembler();

	llvm::DebugFlag = 0;

	//auto triple_str = llvm::sys::getProcessTriple();
	std::string triple_str = "x86_64-pc-windows-msvc-elf";
	auto TT = llvm::Triple(triple_str);

	llvm::orc::JITTargetMachineBuilder JTMB(TT);

	JTMB.setCodeModel(CodeModel::Small);
	JTMB.setRelocationModel(llvm::Reloc::PIC_);

	// Create an LLJIT instance with an ObjectLinkingLayer as the base layer.
	auto J = ExitOnErr(
		LLJITBuilder()
		.setJITTargetMachineBuilder(std::move(JTMB))
		.setObjectLinkingLayerCreator(
			[&](ExecutionSession& ES, const Triple& TT) {
				return std::make_unique<ObjectLinkingLayer>(ES, std::make_unique<jitlink::InProcessMemoryManager>());
			})
		.create());
	
	SymbolMap builtin_syms;
	for (auto& bi : BUILTIN_FUNCS) {
		using namespace llvm;
		auto name = J->mangleAndIntern(SR(bi->ident));
		auto symb  = JITEvaluatedSymbol{
			(JITTargetAddress)bi->builtin_func_ptr,
			JITSymbolFlags::Absolute | JITSymbolFlags::Callable
		};
		builtin_syms.insert({ name, symb });
	}
	J->getMainJITDylib().define(absoluteSymbols(std::move(builtin_syms)));

	auto M = ThreadSafeModule(std::move(modl), ThreadSafeContext(std::move(ctx)));

	ExitOnErr(J->addIRModule(std::move(M)));
	
	typedef void (*main_fp)();
	auto fptr = (main_fp)J->lookup("main")->getAddress();
	if (fptr)
		fptr();
	else
		assert(false);
}
