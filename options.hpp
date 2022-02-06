#pragma once
#include "common.hpp"

struct Options {
	std::string  filename;

	bool         optimized;

	bool         print_ast;
	bool         print_ir;
	bool         print_code;
	
	bool         disasm_print_symbols;
};

inline Options options;
