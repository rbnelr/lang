#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "ir_gen.hpp"
#include "codegen.hpp"
#include "bytecode_vm.hpp"

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

	std::string filename = "test2.la";
	std::string source;
	{
		ZoneScopedN("load_text_file");
		if (!load_text_file(filename.c_str(), &source)) {
			fprintf(stderr, "file not found!\n");
			return 1;
		}
	}

#if TRACY_ENABLE
	for (int profi=0; profi<TRACY_REPEAT; ++profi) {
#endif

	// we need at least one memory block anyway
	// and in case we only end up needing one this could actually help the branch predictor
	// since add_block will never be called in the compiler code this way
	g_allocator.add_block();

	SourceLines lines; // need lines outside of try to allow me to print error messages with line numbers
	try {

		std::vector<Instruction> code;
		{
			ZoneScopedN("compile");
			{
				lines.parse_lines(source.c_str());
			}

			std::vector<Token> tokens;
			{
				tokens = tokenize(source.c_str());
			}

			AST* ast;
			{
				ZoneScopedN("parse");
				Parser parser;
				parser.tok = &tokens[0];
				ast = parser.file();
			}

			//dbg_print(ast);

			std::vector<AST_funcdef*> funcdefs;
			{
				ZoneScopedN("resolve");
				IdentResolver resolve;
				resolve.resolve(ast);

				funcdefs = std::move(resolve.funcs);
			}

			IRGen irgen;
			{
				ZoneScopedN("ir_gen");
				irgen.generate(funcdefs);

				irgen.ir.dbg_print();
			}

			Codegen codegen;
			{
				ZoneScopedN("codegen");
				codegen.generate(irgen.ir.code);

				codegen.dbg_print();

				code = std::move(codegen.code);
			}
		}

	//#ifndef TRACY_ENABLE
		VM vm;
		{
			ZoneScopedN("vm.execute");
			vm.execute(code.data(), code.size(), 0);
		}
	//#endif
	}
	catch (CompilerExcept& ex) {
		ex.print(filename.c_str(), lines);
	}
	catch (RuntimeExcept& ex) {
		ex.print();
	}
	catch (...) {
		fprintf(stderr, "Unknown exception!");
	}

	g_allocator.reset();

#if TRACY_ENABLE
	}
#endif

	return 0;
}
