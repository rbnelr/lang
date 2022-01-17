#pragma once

struct Options {
	std::string  filename  = "test.la";

	bool         optimized = 1;

#ifdef TRACY_ENABLE
	bool         print_ast  = false;
	bool         print_ir   = false;
	bool         print_code = false;
#else
	bool         print_ast  = 0;
	bool         print_ir   = true;
	bool         print_code = true;
#endif
	
	bool         disasm_print_symbols = true;
};

inline Options options;
