#include "common.hpp"
#include "tokenizer.hpp"

int main (char** argv, int argc) {
	enable_console_ansi_color_codes();

	std::string filename = "test.la";

	std::string source;
	if (!kiss::load_text_file(filename.c_str(), &source)) {
		fprintf(stderr, "file not found!\n");
		return 1;
	}

	Tokenizer tok;
	try {
		{
			ZoneScopedN("tok.init_source");
			tok.init_source(source.c_str(), filename.c_str());
		}

		ZoneScopedN("tokenize");

		printf("Tokenizer:\n");
		while (tok.peek() != T_EOF) {
			tok.get();
		}
		
	} catch (Tokenizer::Exception& ex) {
		ex.print(tok);
	} catch (std::exception& ex) {
		fprintf(stderr, "Unknown exception: %s", ex.what());
	}

	return 0;
}
