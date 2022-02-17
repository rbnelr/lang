#include "common.hpp"
#include "frontend/lexer.hpp"
#include <random>

//#include <chrono>

namespace profiling {
	
struct Random {
	std::default_random_engine generator;

	Random (unsigned int seed): generator{seed} {} // seed with value
	
	inline unsigned int uniform (unsigned int max=UINT_MAX) {
		std::uniform_int_distribution<unsigned int> distribution (0, max);
		return distribution(generator);
	}
	inline int uniform (int min, int max) {
		std::uniform_int_distribution<int> distribution (min, max);
		return distribution(generator);
	}
	
	inline float uniform (float min, float max) {
		std::uniform_real_distribution<float> distribution (min, max);
		return distribution(generator);
	}

	inline char uniform_char (strview chars) {
		std::uniform_int_distribution<int> distribution (0, (int)chars.size()-1);
		return chars[ distribution(generator) ];
	}
};

void lex_file_simple_tok (char const* filename, size_t size = MB * 64) {
	printf("writing %s...\n", filename);
	
	std::string str;

	Random rand(0);

	int line_len = rand.uniform(20, 120);
	size_t line_start = 0;

	while (str.size() < size) {

		if (str.size() - line_start >= line_len) {
			line_len = rand.uniform(20, 120);
			line_start = str.size();
			str.append("\r\n");
		}

		int first = T_COLON;
		int count = T_MODEQ - T_COLON;

		auto tok = (TokenType)rand.uniform(first, first + count - 1);

		str.append(TokenType_char[tok]);

		// some tokens might be misread if there are no spaces, shouldn't break lexer profiling, but let's do it anyway
		int spaces = rand.uniform(1, 2);
		for (int i=0; i<spaces; ++i)
			str.append(" ");
	}

	kiss::save_text_file(filename, str);
}
void lex_file_whitespace (char const* filename, bool comments=true, size_t size = MB * 64) {
	printf("writing %s...\n", filename);
	
	std::string str;

	Random rand(0);

	int line_len = rand.uniform(20, 120);
	size_t line_start = 0;

	while (str.size() < size) {
		if (str.size() - line_start >= line_len) {
			line_len = rand.uniform(20, 120);
			line_start = str.size();
			str.append("\r\n");
		}

		auto k = rand.uniform(0, comments ? 20 : 18);
		if (k <= 14) {
			int spaces = rand.uniform(1, 3);
			for (int i=0; i<spaces; ++i)
				str.append(" ");
		}
		else if (k <= 18) {
			int tabs = rand.uniform(1, 6);
			for (int i=0; i<tabs; ++i)
				str.append("\t");
		}
		else if (k <= 19) {
			int len = rand.uniform(4, 120);

			str.append("//");
			
			for (int i=0; i<len; ++i)
				str.push_back( rand.uniform_char("Ka uDd ft^&3+ uagd \t") );
			
			str.append("\n");
		}
		else {
			int len = rand.uniform(4, 120);
			str.append("/*");
			
			for (int i=0; i<len; ++i)
				str.push_back( rand.uniform_char("Ka uDd ft^&3+ uagd \t \n") );

			str.append("*/");
		}
	}

	kiss::save_text_file(filename, str);
}
void lex_file_numbers (char const* filename, size_t size = MB * 64) {
	printf("writing %s...\n", filename);
	
	std::string str;

	Random rand(0);

	int line_len = rand.uniform(20, 120);
	size_t line_start = 0;

	while (str.size() < size) {
		if (str.size() - line_start >= line_len) {
			line_len = rand.uniform(20, 120);
			line_start = str.size();
			str.append("\r\n");
		}

		auto k = rand.uniform(0, 5);
		if (k <= 1) {
			int num = rand.uniform(0, 6);
			str.append(prints("%d", num));
		}
		else if (k <= 2) {
			int num = rand.uniform(0, INT_MAX);
			str.append(prints("%d", num));
		}
		else if (k <= 4) {
			float num = rand.uniform(0.0f, 3.0f);
			str.append(prints("%.3f", num));
		}
		else {
			float num = rand.uniform(0.0f, (float)INT_MAX);
			str.append(prints("%7.2f", num));
		}
		
		int spaces = rand.uniform(1, 2);
		for (int i=0; i<spaces; ++i)
			str.append(" ");
	}

	kiss::save_text_file(filename, str);
}
void lex_file_identifiers (char const* filename, size_t size = MB * 64) {
	printf("writing %s...\n", filename);
	
	std::string str;

	Random rand(0);

	int line_len = rand.uniform(20, 120);
	size_t line_start = 0;

	while (str.size() < size) {
		if (str.size() - line_start >= line_len) {
			line_len = rand.uniform(20, 120);
			line_start = str.size();
			str.append("\r\n");
		}

		auto start = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ_";
		auto middle = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ_0123456789";

		auto k = rand.uniform(0, 4);
		if (k <= 2) {
			int len = rand.uniform(1, 5);
			
			str.push_back(rand.uniform_char(start));
			for (int i=1; i<len; ++i) {
				str.push_back(rand.uniform_char(middle));
			}
		}
		else if (k <= 3) {
			int len = rand.uniform(1, 20);
			
			str.push_back(rand.uniform_char(start));
			for (int i=1; i<len; ++i) {
				str.push_back(rand.uniform_char(middle));
			}
		}
		else {
			int first = T_FUNC;
			int count = T_GOTO - T_FUNC;

			auto tok = (TokenType)rand.uniform(first, first + count - 1);
			
			str.append(TokenType_char[tok]);
		}

		int spaces = rand.uniform(1, 2);
		for (int i=0; i<spaces; ++i)
			str.append(" ");
	}

	kiss::save_text_file(filename, str);
}
void lex_file_realcode (char const* filename, size_t size = MB * 64) {
	printf("writing %s...\n", filename);
	
	std::string str;

	std::string test1 = kiss::load_text_file("test_100k.la");

	while (str.size() < size) {
		str.append(test1);
	}

	kiss::save_text_file(filename, str);
}

_NOINLINE void profile_lexer (char const* filename) {
	std::string file = kiss::load_text_file(filename);

	int repeat = 5;
	
	//auto start = std::chrono::steady_clock::now();
	auto timer = Timer::start();
	for (int i=0; i<repeat; ++i) {
		Lexer lex{file.c_str()};

		while (lex[0].type != T_EOF) {
			lex.eat();
		}
	}
	float time = timer.end() / (float)repeat;
	//auto end = std::chrono::steady_clock::now();
	//std::chrono::duration<float> dur = end - start;
	//auto time = dur.count() / (float)repeat;

	printf("%30s: %8llu MB  in  %6.2f ms  ->  %6.2f MB/s\n", filename,
		file.size() / MB,
		time * 1000,
		(float)file.size() / time / (float)MB);
}

void lex_profile () {
	bool create_files = 0;

	if (create_files) {
		lex_file_simple_tok ("prof_lex0_simple_toks.la");
		lex_file_whitespace ("prof_lex1_whitespace_comm.la", true);
		lex_file_whitespace ("prof_lex2_whitespace.la",      false);
		lex_file_numbers    ("prof_lex3_numbers.la");
		lex_file_identifiers("prof_lex4_identifiers.la");
		lex_file_realcode   ("prof_lex5_realcode.la");
	}

	print_seperator("profiling files");
	
	profile_lexer("prof_lex0_simple_toks.la");
	profile_lexer("prof_lex1_whitespace_comm.la");
	profile_lexer("prof_lex2_whitespace.la");
	profile_lexer("prof_lex3_numbers.la");
	profile_lexer("prof_lex4_identifiers.la");
	profile_lexer("test_100k.la");
	profile_lexer("prof_lex5_realcode.la");
}

}
