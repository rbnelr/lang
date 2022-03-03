#pragma once
#include "common.hpp"

struct Options {
	std::string  filename;

	bool         optimized         = 1;

	bool         print_ast         = 0;
	bool         print_ir          = 0;
	bool         print_disasm      = 0;
	
	bool         disasm_symbols    = 1;
	bool         disasm_code_bytes = 1;
};

inline Options options;
