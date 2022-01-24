#pragma once

struct Options {
	std::string  filename  = "test.la";

	bool         optimized = 1;

#ifdef TRACY_ENABLE
	bool         print_ast  = 0;
	bool         print_ir   = 0;
	bool         print_code = 0;
#else
	bool         print_ast  = 0;
	bool         print_ir   = 1;
	bool         print_code = 1;
#endif
	
	bool         disasm_print_symbols = true;
};

inline Options options;
