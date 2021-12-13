#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "ast_exec.hpp"

int main (char** argv, int argc) {
	enable_console_ansi_color_codes();

	std::string filename = "test.la";

	std::string source;
	if (!kiss::load_text_file(filename.c_str(), &source)) {
		fprintf(stderr, "file not found!\n");
		return 1;
	}

#if TRACY_ENABLE
	for (int profi=0; profi<1000; ++profi) {
#endif

	SourceLines lines; // need lines outside of try to allow me to print error messages with line numbers
	try {
		lines.parse_lines(source.c_str());

		auto tokens = tokenize(source.c_str());

		ZoneScopedN("interpret");

		Parser parser;
		parser.tok = &tokens[0];
		auto ast = parser.file();

		dbg_print(ast.get());

		printf("--------------------\n");

		Interpreter interp;
		interp.execute(ast.get());
	}
	catch (MyException& ex) {
		ex.print(filename.c_str(), lines);
	}
	catch (...) {
		fprintf(stderr, "Unknown exception!");
	}

#if TRACY_ENABLE
	}
#endif

	return 0;
}
