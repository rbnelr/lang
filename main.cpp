#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "resolve.hpp"
#include "ast_exec.hpp"

int main (int argc, const char** argv) {
	enable_console_ansi_color_codes();

	std::string filename = "test3.la";
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

		AST* ast;
		IdentiferIDs ident_ids;
		{
			ZoneScopedN("compile");
			{
				lines.parse_lines(source.c_str());
			}

			std::vector<Token> tokens;
			{
				tokens = tokenize(source.c_str(), ident_ids);
			}

			{
				ZoneScopedN("parse");
				Parser parser;
				parser.tok = &tokens[0];
				ast = parser.file();
			}

			dbg_print(ast);

			{
				ZoneScopedN("resolve_idents");
				IdentifierResolve ires;
				ires.resolve_idents(ast);
			}
		}

	#ifndef TRACY_ENABLE
		printf("--------------------\n");
		
		{
			ZoneScopedN("interpret AST");
			Interpreter interp;
		
			Value retval;
			interp.execute(ast, &retval, 0);
		}
	#endif
	}
	catch (MyException& ex) {
		ex.print(filename.c_str(), lines);
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
