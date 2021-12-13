#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"

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

	Tokenized   tokens;
	Parser      parser;
	try {
		tokens = tokenize(source.c_str());

		ZoneScopedN("interpret");

		parser.tok = &tokens.tokens[0];
		auto ast = parser.file();

		dbg_print(ast.get());
	}
	catch (Exception& ex) {
		ex.print(filename.c_str(), tokens.lines);
	}
	catch (...) {
		fprintf(stderr, "Unknown exception!");
	}

#if TRACY_ENABLE
	}
#endif

	return 0;
}
