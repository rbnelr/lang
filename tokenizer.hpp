#pragma once
#include "common.hpp"
#include "errors.hpp"

namespace parse {
	//// Check char class

	constexpr inline bool is_decimal_c (char c) {
		return c >= '0' && c <= '9';
	}
	constexpr inline bool is_hex_c (char c) {
		return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
	}
	constexpr inline bool is_alpha_c (char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
	}
	constexpr inline bool is_ident_start_c (char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
	}
	constexpr inline bool is_ident_c (char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || (c >= '0' && c <= '9');
	}
	constexpr inline bool is_whitespace_c (char c) {
		return c == ' ' || c == '\t';
	}

	inline bool is_newline (char const* c) {
		return *c == '\n' || *c == '\r';
	}

	// skips "\n" or "\r" or "\r\n" or "\n\r"
	inline bool newline (char const*& c) {
		if (!is_newline(c))
			return false;

		char ch = *c;
		c++;

		// "\n" "\r" "\n\r" "\r\n" each count as one newline whilte "\n\n" "\r\r" count as two
		// ie. this code should even handle files with inconsistent newlines somewhat reasonably
		if (is_newline(c) && ch != *c)
			c++;
		return true;
	}

	// skips "-012345"
	// returns int in <out_int>
	inline bool integer (const char*& c, int* out_int) {
		const char* cur = c;

		bool neg = false;
		if (*cur == '-') {
			neg = true;
			cur++;
		} else if (*cur == '+') {
			cur++;
		}

		if (*cur < '0' || *cur > '9')
			return false;

		unsigned int out = 0;
		while (*cur >= '0' && *cur <= '9') {
			out *= 10;
			out += *cur++ - '0';
		}

		*out_int = neg ? -(int)out : (int)out;
		c = cur;
		return true;
	}

	inline bool parse_double (const char*& c, double* out) {
		char const* cur = c;
		double val = strtod(c, (char**)&cur); // need to cast away const for strtod api

		if (cur > c) {
			*out = val;
			c = cur;
			return true;
		}
		return false; // parsing error
	}
}

enum TokenType {
	T_EOF=0, // end of file

	T_PLUS,        // +
	T_MINUS,       // -
	T_MULTIPLY,    // *
	T_DIVIDE,      // /
	T_PAREN_OPEN,  // (
	T_PAREN_CLOSE, // )
	T_COMMA,       // ,
	T_EQUALS,      // =
	T_SEMICOLON,   // ;

	T_LITERAL_INT,
	// starts with [0-9], then anything that strtod() accepts
	// note: no sign is ever parsed, '+' or '-' are greedily tokenizes as T_PLUS and T_MINUS
	// is parsed for its float value, which gets stored in the Token struct
	T_LITERAL_FLOAT,

	// starts with  '_' or [a-Z]  and then any number of  '_' or [a-Z] or [0-9]
	T_IDENTIFIER,
};
inline constexpr const char* TokenType_str[] = {
	"T_EOF",
	
	"T_PLUS",
	"T_MINUS",
	"T_MULTIPLY",
	"T_DIVIDE",
	"T_POWER",
	"T_PAREN_OPEN",
	"T_PAREN_CLOSE",
	"T_COMMA",
	"T_EQUALS",
	
	"T_LITERAL_INT",
	"T_LITERAL_FLOAT",
	
	"T_IDENTIFIER",
};

struct Token {
	TokenType  type;
	strview    text;
	size_t     lineno;

	union {
		uint64_t  value_int; // T_LITERAL_INT
		double    value_flt; // T_LITERAL_FLOAT
	};

	Token () {}
};

struct Tokenizer {
	const char*          cur;
	const char*          cur_line;

	Token                buf[2]; // need 2 tokens pre-tokenized to allow for peek() with 1 token lookahead

	std::vector<strview> lines;

	//std::string last_error;
	void throw_error (const char* errstr, const char* start, const char* cur, size_t lineno) {
		throw Exception{ errstr, start, cur, lineno };
	}

	void init_source (const char* src) {
		cur = src;
		cur_line = src;

		lines.reserve(1024);
		prescan_line();

		//last_error = "";

		buf[0] = parse_next_token();
		buf[1] = parse_next_token();
	}

	TokenType peek (int lookahead=0) {
		assert(lookahead >= 0 && lookahead < ARRLEN(buf));
		return buf[lookahead].type;
	}

	Token get () {
		Token tok;
		tok    = buf[0];
		buf[0] = buf[1];

		buf[1] = parse_next_token();

		//printf("%-18s : ", TokenType_str[tok.type]);
		//fwrite(tok.text.data(), 1, (int)tok.text.size(), stdout);
		//printf("\n");

		return tok;
	}

	bool eat (TokenType type) {
		if (buf[0].type != type)
			return false;
		get();
		return true;
	}

	// need to know the end of the current line while processing tokens in it to allow printing the whole line on syntax errors
	void prescan_line () {
		const char* c = cur;
		while (*c != '\n' && *c != '\r' && *c != '\0')
			c++; // skip anything until newline or EOF

		lines.emplace_back(strview(cur_line, c - cur_line));
	}
	bool newline () {
		if (parse::newline(cur)) {

			cur_line = cur;
			prescan_line();

			return true;
		}
		return false;
	}

	Token parse_next_token () {
		using namespace parse;

		for (;;) {
			if (is_whitespace_c(*cur)) {
				cur++;
				continue;
			}

			if (cur[0] == '/' && cur[1] == '/') {
				cur += 2; // skip "//"

				while (*cur != '\n' && *cur != '\r' && *cur != '\0')
					cur++; // skip anything until newline or EOF

				// fallthrough to newline
			}
			else if (cur[0] == '/' && cur[1] == '*') {
				cur += 2; // skip "/*"
				
				size_t depth = 1;
				while (depth > 0) {
					if (*cur == '\0') {
						// TODO: also add note about block comment open location to error
						throw_error("end of file in block comment", cur, cur+1, lines.size()-1);
					}
					else if (newline()) {
						
					}
					else if (cur[0] == '/' && cur[1] == '*') {
						cur += 2; // skip "/*"
						depth++;
					}
					else if (cur[0] == '*' && cur[1] == '/') {
						cur += 2; // skip "*/"
						depth--;
					}
					else {
						cur++;
					}
				}
				continue;
			}
			else if (cur[0] == '*' && cur[1] == '/') {
				throw_error("unexpected block comment close", cur, cur+2, lines.size()-1);
			}

			if (newline())
				continue;

			// non-whitespace character outside of comment -> start of a token
			break;
		}

		Token tok;
		tok.lineno = lines.size()-1;

		if (*cur == '\0') {
			tok.type = T_EOF;
			tok.text = strview(cur, 1);
			return tok;
		}

		const char* start = cur;
		switch (*cur) {
			case '+': tok.type = T_PLUS;          break;
			case '-': tok.type = T_MINUS;         break;
			case '*': tok.type = T_MULTIPLY;      break;
			case '/': tok.type = T_DIVIDE;        break;

			case '(': tok.type = T_PAREN_OPEN;    break;
			case ')': tok.type = T_PAREN_CLOSE;   break;
			case ',': tok.type = T_COMMA;         break;

			case '=': tok.type = T_EQUALS;        break;
			case ';': tok.type = T_SEMICOLON;     break;

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {
				
				if (!parse_double(cur, &tok.value_flt)) {
					throw_error("number parse error", start, start+1, tok.lineno);
				}

				tok.type = T_LITERAL_FLOAT;
				tok.text = strview(start, cur - start);
				return tok;
			}

			default: {
				if (is_ident_start_c(*cur)) {

					while (is_ident_c(*cur))
						cur++; // find end of identifier

					tok.type = T_IDENTIFIER;
					tok.text = strview(start, cur - start);

					return tok;
				}
				else {
					throw_error("unknown token", start, start+1, tok.lineno);
				}
			}
		}

		cur++; // single-char token
		tok.text = strview(start, cur - start);
		return tok;
	}
};
