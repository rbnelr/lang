#include "common.hpp"
#include "errors.hpp"
#include "tokenizer.hpp"
#include "interpreter.hpp"

int main (char** argv, int argc) {
	enable_console_ansi_color_codes();

	std::string filename = "test.la";

	std::string source;
	if (!kiss::load_text_file(filename.c_str(), &source)) {
		fprintf(stderr, "file not found!\n");
		return 1;
	}

	Interpreter interp;
	try {
		{
			ZoneScopedN("tok.init_source");
			interp.tok.init_source(source.c_str());
		}

		ZoneScopedN("interpret");

		auto val = interp.statement();
		printf("%g\n", val.val);
	}
	catch (Exception& ex) {
		ex.print(filename.c_str(), interp.tok.lines);
	}
	catch (...) {
		fprintf(stderr, "Unknown exception!");
	}

	return 0;
}
