#pragma once
#include "common.hpp"
#include "frontend/parser.hpp"

namespace llvm {
	class LLVMContext;
	class Module;
	class Function;
}

struct Module {
	llvm::LLVMContext* ctx  = nullptr;
	llvm::Module*      modl = nullptr;

	Module () {}

	Module (llvm::LLVMContext* ctx, llvm::Module* modl): ctx{ctx}, modl{modl} {}

	Module (Module&& r) {
		ctx  = r.ctx;
		modl = r.modl;
		
		r.ctx  = nullptr;
		r.modl = nullptr;
	}
	Module& operator= (Module&& r) {
		ctx  = r.ctx;
		modl = r.modl;
		
		r.ctx  = nullptr;
		r.modl = nullptr;

		return *this;
	}

	~Module ();
};

Module llvm_gen_module (AST_Module& modl);

// unfortunately has to steal ownership of LLVM_Module
// since the LLVM example code for some reason can't simply use a module, they take a unique_ptr to it by value
void llvm_jit_and_exec (Module modl);
