#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "resolver.hpp"
#include "ir_gen.hpp"
#include "ir_opt.hpp"
#include "codegen.hpp"
#include "bytecode_vm.hpp"

#include "llvm_backend.hpp"


bool compile () {

	std::string tok;
	{
		ZoneScopedN("load_text_file");
		if (!load_text_file(options.filename.c_str(), &tok)) {
			fprintf(stderr, "file not found!\n");
			return false;
		}
	}

	VM vm; // don't recreate when profling due to stack allocation

#if TRACY_ENABLE
	for (int profi=0; profi<TRACY_REPEAT; ++profi) {
	#endif

		// we need at least one memory block anyway
		// and in case we only end up needing one this could actually help the branch predictor
		// since add_block will never be called in the compiler code this way
		g_allocator.add_block();

		SourceLines lines; // need lines outside of try to allow me to print error messages with line numbers
		try {

			#define LLVM 1

			ZoneScopedN("compile");
			{
				lines.parse_lines(tok.c_str());
			}

			std::vector<Token> tokens;
			{
				tokens = tokenize(tok.c_str());
			}

			AST* ast;
			{
				ZoneScopedN("parse");
				Parser parser;
				parser.tok = &tokens[0];
				ast = parser.file();

				if (options.print_ast) { // print AST
					print_seperator("AST:");
					dbg_print(ast);
				}
			}

			std::vector<AST_funcdef*> funcdefs;
			{
				ZoneScopedN("resolve");
				IdentResolver resolve;
				resolve.resolve_ast(ast);

				funcdefs = std::move(resolve.funcs);
			}

		#if LLVM
			{
				llvm::Module* llvm_modl = llvm_gen_module(options.filename, funcdefs);
				defer( llvm_free_module(llvm_modl); );
			
				//#ifndef TRACY_ENABLE
				{
					ZoneScopedN("llvm exe");
					llvm_jit_and_exec(llvm_modl);
				}
				//#endif
			}
		#else
			std::vector<Instruction> code;
			{
				IR::IRGen irgen = { funcdefs };
				{
					irgen.generate();
				}
				
				{
					IR::ir_opt(irgen);
				}
				
				{
					ZoneScopedN("codegen");
					Codegen codegen;
					codegen.generate(irgen);
				
					if (options.print_code)
						codegen.dbg_print();
				
					code = std::move(codegen.code);
				}
			}

			#ifndef TRACY_ENABLE
			{
				ZoneScopedN("vm.execute");
				vm.execute(code.data(), code.size(), 0);
			}
			#endif
		#endif
		}
		catch (CompilerExcept& ex) {
			ex.print(options.filename.c_str(), lines);
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
	//fib(50);
	//pascal_tri(13);

	enable_console_ansi_color_codes();

	setvbuf(stderr, nullptr, _IOFBF, BUFSIZ);

	compile();

	return 0;
}
