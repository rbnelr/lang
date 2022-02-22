#pragma once
#include "common.hpp"
#include "frontend/parser.hpp"

namespace llvm {
	class LLVMContext;
	class Module;
	class Function;
}

struct llvmModule {
	llvm::LLVMContext* ctx  = nullptr;
	llvm::Module*      modl = nullptr;
	
	llvmModule () {}
	llvmModule (llvm::LLVMContext* ctx, llvm::Module* modl): ctx{ctx}, modl{modl} {}

	~llvmModule ();
	
	void _move (llvmModule& r) {
		ctx  = r.ctx ; r.ctx  = nullptr;
		modl = r.modl; r.modl = nullptr;
	}
	llvmModule (llvmModule&& r) { _move(r); }
	llvmModule& operator= (llvmModule&& r) { _move(r); return *this; }

};

llvmModule llvm_gen_module (AST_Module& modl);

// unfortunately has to steal ownership of LLVM_Module
// since the LLVM example code for some reason can't simply use a module, they take a unique_ptr to it by value
void llvm_jit_and_exec (llvmModule& modl);
