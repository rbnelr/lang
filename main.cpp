#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "optimize.hpp"
#include "ast_exec.hpp"

int main (int argc, const char** argv) {
	enable_console_ansi_color_codes();

	std::string filename = "test.la";
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

#if USE_ALLOCATOR
	//g_allocator.add_block();
#endif

	SourceLines lines; // need lines outside of try to allow me to print error messages with line numbers
	try {

		ast_ptr ast;
		IdentiferIDs ident_ids;
		{
			ZoneScopedN("compile");
			lines.parse_lines(source.c_str());

			auto tokens = tokenize(source.c_str(), ident_ids);

			Parser parser;
			parser.tok = &tokens[0];
			ast = parser.file();

			//ast->dbg_print(0);

			{
				ZoneScopedN("map_vars");
				OptimizePasses opt;
				opt.map_vars(ast.get());
			}
		}

	//#ifndef TRACY_ENABLE
		printf("--------------------\n");
		
		{
			ZoneScopedN("interpret AST");
			Interpreter interp;
		
			Value retval;
			interp.execute(ast.get(), &retval);
		}
	//#endif
	}
	catch (MyException& ex) {
		ex.print(filename.c_str(), lines);
	}
	catch (...) {
		fprintf(stderr, "Unknown exception!");
	}

#if USE_ALLOCATOR
	g_allocator.reset();
#endif

#if TRACY_ENABLE
	}
#endif

	return 0;
}
