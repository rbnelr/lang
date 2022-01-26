#pragma once
#include "common.hpp"
#include "parser.hpp"

namespace llvm {
	class Module;
	class Function;
}

llvm::Module* llvm_gen_module (strview const& filename, std::vector<AST_funcdef*>& funcdefs, SourceLines const& lines);
void llvm_free_module (llvm::Module* modl); // do this to hide llvm headers from main

void llvm_jit_and_exec (llvm::Module* modl);
