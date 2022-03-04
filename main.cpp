#include "common.hpp"

#include "options.hpp"
#include "errors.hpp"

#include "frontend/lexer.hpp"
#include "frontend/parser.hpp"
#include "frontend/semantic.hpp"

#include "backend_llvm/llvm_backend.hpp"

#include "profiling.hpp"

#define TRACY_REPEAT 1000

void set_options (int argc, const char** argv) {
#if 1
	options.filename  = "test2.la";

	options.optimized = 1;

#ifndef NDEBUG
	options.print_ast         = 0;
	options.print_ir          = 1;
	options.print_disasm      = 1;
#endif
	
	options.disasm_symbols    = 1;
	options.disasm_code_bytes = 1;
	
#else
	if (argc <= 1) {
		fprintf(stderr, "usage: <compiler> [-log ast] [-log ir] [-log code] <source filepath>\n");
		exit(1);
	}

	int argi = 1;
	while (argi < argc) {
		const char* option = argv[argi++];

		if (option[0] == '-') {
			if (strcmp(option, "-log") == 0) {
				if (argi >= argc) {
					fprintf(stderr, "-log options needs argument like '-log <arg>' where <arg> is 'ast', 'ir' or 'code'\n");
					exit(1);
				}
				const char* arg = argv[argi++];
			
				if      (strcmp(arg, "ast")    == 0) options.print_ast    = true;
				else if (strcmp(arg, "ir")     == 0) options.print_ir     = true;
				else if (strcmp(arg, "disasm") == 0) options.print_disasm = true;
			}
			else {
				fprintf(stderr, "unknown option '%s'\n", option);
				exit(1);
			}
		}
		else {
			options.filename = option;
		}
	}
#endif
}

bool compile () {

	std::string source;
	{
		ZoneScopedN("load_text_file");
		if (!load_text_file(options.filename.c_str(), &source)) {
			fprintf(stderr, "file not found!\n");
			return false;
		}
	}
	
#if TRACY_ENABLE
	for (int profi=0; profi<TRACY_REPEAT; ++profi) {
#endif
		ZoneScopedN("compile");

		// we need at least one memory block anyway
		g_allocator.add_block();

		AST_Module modl;
		modl.filename = options.filename;

		try {
			{
				ZoneScopedNC("frontend", tracy::Color::CadetBlue);

				parse(modl, source);
				semantic_analysis(modl);
			}

			{
				ZoneScopedNC("backend", tracy::Color::Burlywood);
			
				auto llvm_modl = llvm_gen_module(modl);
				
				//#ifndef TRACY_ENABLE
				{
					ZoneScopedN("llvm_jit_and_exec");
					llvm_jit_and_exec(llvm_modl);
				}
				//#endif
			}
		}
		catch (CompilerExcept& ex) {
			ex.print(modl.filename.c_str());
			return false;
		}
		catch (RuntimeExcept& ex) {
			ex.print();
			return false;
		}
		catch (...) {
			fprintf(stderr, "\nUnknown exception!");
			return false;
		}
		
		g_allocator.reset();

#if TRACY_ENABLE
	}
#endif

	return true;
}


_NOINLINE void fib (int n) {
	int a = 0;
	int b = 1;

	printf("%d %d", a, b);

	for (int i = 0; i < n-2; i++) {
		int c = a + b;
		printf(" %d", c);
		a = b;
		b = c;
	}

	printf("\n");
}
_NOINLINE int pascal_tri (int i, int row) {
	if (i < 0 || i > row)
		return 0;

	if (row == 0)
		return 1;

	int a = pascal_tri(i-1, row-1);
	int b = pascal_tri(i  , row-1);
	return a + b;
}
_NOINLINE void pascal_tri (int rows) {
	for (int row=0; row < rows; ++row) {

		for (int i=0; i < rows-1 - row; ++i)
			printf("  ");

		for (int i=0; i<=row; ++i) {
			int val = pascal_tri(i, row);
			printf("%03d ", val);
		}

		printf("\n");
	}
}

int main (int argc, const char** argv) {
	set_options(argc, argv);

	//fib(50);
	//pascal_tri(13);
	
	//profiling::lex_profile();
	//return 0;

	enable_console_ansi_color_codes();

	setvbuf(stderr, nullptr, _IOFBF, BUFSIZ);

	compile();

	return 0;
}
