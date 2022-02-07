#include "common.hpp"

#include "options.hpp"

#include "frontend/errors.hpp"
#include "frontend/tokenizer.hpp"
#include "frontend/parser.hpp"
#include "frontend/semantic.hpp"

#include "backend_llvm/llvm_backend.hpp"

#define TRACY_REPEAT 1000

void set_options () {
	options.filename  = "test_100k.la";

	options.optimized = 1;

#ifdef NDEBUG
	options.print_ast  = 0;
	options.print_ir   = 0;
	options.print_code = 0;
#else
	options.print_ast  = 1;
	options.print_ir   = 1;
	options.print_code = 1;
#endif
	
	options.disasm_print_symbols = true;
}

bool compile () {

	set_options();

	std::string tok;
	{
		ZoneScopedN("load_text_file");
		if (!load_text_file(options.filename.c_str(), &tok)) {
			fprintf(stderr, "file not found!\n");
			return false;
		}
	}
	
	int repeat = 100;
	printf("averaged over %d runs\n", repeat);
	printf("bufsz, time[ms], tokenize_count, bufmemsz[KB]\n");

	int bufsz = 0;
	for (int i=0;; ++i) {

		int sz = (int)std::powf(2.0f, 0.13f * (float)i);
		if (sz <= bufsz)
			continue;
		bufsz = sz;

		if (bufsz > 1000000)
			break;

		float total = 0.0f;

		for (int j=0; j<repeat; ++j) {

//#if TRACY_ENABLE
//	for (int profi=0; profi<TRACY_REPEAT; ++profi) {
//#endif

		// we need at least one memory block anyway
		g_allocator.add_block();

		AST_Module modl;
		modl.filename = options.filename;

		try {

			ZoneScopedN("compile");
			
			{
				ZoneScopedNC("frontend", tracy::Color::CadetBlue);
				
				auto t = Timer::start();
				parse(modl, tok.c_str(), bufsz);
				total += t.end();

				semantic_analysis(modl);
			}

			//{
			//	ZoneScopedNC("backend", tracy::Color::Burlywood);
			//
			//	llvm::Module* llvm_modl = llvm_gen_module(modl, lines);
			//	defer( llvm_free_module(llvm_modl); );
			//
			//	//#ifndef TRACY_ENABLE
			//	{
			//		ZoneScopedN("llvm_jit_and_exec");
			//		llvm_jit_and_exec(llvm_modl);
			//	}
			//	//#endif
			//}
		}
		catch (CompilerExcept& ex) {
			ex.print(modl.filename.c_str(), modl.lines);
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

//#if TRACY_ENABLE
//	}
//#endif
		}
		printf("%d, %g, %d, %f\n", bufsz, total/(float)repeat * 1000.0f, tokenize_count, bufsz * sizeof(Token) / 1024.0f);
	}

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
	//fib(50);
	//pascal_tri(13);

	enable_console_ansi_color_codes();

	setvbuf(stderr, nullptr, _IOFBF, BUFSIZ);

	compile();

	return 0;
}
