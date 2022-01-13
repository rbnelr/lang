#pragma once
#include "common.hpp"
#include "parser.hpp"

namespace llvm {
	class Module;
	class Function;
}

// init uses globals to store the llvm context instead of returning it
// to hide the implementation to avoid headers being pulled into main
void llvm_init ();

llvm::Module* llvm_gen_module (std::vector<AST_funcdef*>& funcdefs);
void llvm_free_module (llvm::Module* modl); // do this to hide llvm headers from main

void llvm_jit_and_exec (llvm::Module* modl);
