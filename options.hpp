#pragma once
#include "common.hpp"

struct Options {
	std::string  filename;

	bool         optimized  = 1;

	bool         print_ast  = 0;
	bool         print_ir   = 0;
	bool         print_code = 0;
	
	bool         disasm_print_symbols = 1;
};

inline Options options;
