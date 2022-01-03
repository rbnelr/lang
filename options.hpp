#pragma once

struct Options {
	std::string  filename  = "test5.la";

	bool         optimized = 1;

#ifdef TRACY_ENABLE
	bool         print_ast  = false;
	bool         print_ir   = false;
	bool         print_code = false;
#else
	bool         print_ast  = true;
	bool         print_ir   = true;
	bool         print_code = true;
#endif
};

Options options;
