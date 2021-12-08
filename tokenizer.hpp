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
	inline bool parse_integer (const char*& c, int64_t* out_int) {
		const char* cur = c;
		if (*cur < '0' || *cur > '9')
			return false;

		int64_t out = 0;
		while (*cur >= '0' && *cur <= '9') {
			out *= 10;
			out += *cur++ - '0';
		}

		*out_int = (int)out;
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

	T_PLUS,          // +
	T_MINUS,         // -
	T_MULTIPLY,      // *
	T_DIVIDE,        // /
	T_EQUALS,        // =
	T_SEMICOLON,     // ;
	T_COMMA,         // ,

	T_PAREN_OPEN,    // (
	T_PAREN_CLOSE,   // )
	T_BLOCK_OPEN,    // {
	T_BLOCK_CLOSE,   // }
	T_INDEX_OPEN,    // [
	T_INDEX_CLOSE,   // ]

	T_LITERAL_INT,
	T_LITERAL_FLOAT,
	T_LITERAL_STRING,

	// starts with  '_' or [a-Z]  and then any number of  '_' or [a-Z] or [0-9]
	T_IDENTIFIER,

	T_FOR,
};
inline constexpr const char* TokenType_str[] = {
	"T_EOF",

	"T_PLUS",       
	"T_MINUS",      
	"T_MULTIPLY",   
	"T_DIVIDE",     
	"T_EQUALS",     
	"T_SEMICOLON",  
	"T_COMMA",      

	"T_PAREN_OPEN", 
	"T_PAREN_CLOSE",
	"T_BLOCK_OPEN", 
	"T_BLOCK_CLOSE",
	"T_INDEX_OPEN", 
	"T_INDEX_CLOSE",
	
	"T_LITERAL_INT",
	"T_LITERAL_FLOAT",
	"T_LITERAL_STRING",
	
	"T_IDENTIFIER",

	"T_FOR",
};
inline constexpr const char* TokenType_char[] = {
	"\\0",

	"+",
	"-",
	"*",
	"/",
	"=",
	";",
	",",
	
	"(",
	")",
	"{",
	"}",
	"[",
	"]",

	"int",
	"flt",
	"str",

	"identifier",

	"for",
};

struct Token {
	TokenType  type;
	strview    text;
	size_t     lineno;

	union {
		int64_t   value_int; // T_LITERAL_INT
		double    value_flt; // T_LITERAL_FLOAT
		strview   value_str;
	};

	Token () {}
};

struct Tokenized {
	std::vector<Token>   tokens;
	std::vector<strview> lines;
};
Tokenized tokenize (const char* src) {
	ZoneScoped;

	Tokenized res;

	res.tokens.reserve(4096);
	res.lines.reserve(4096);

	const char* cur = src;
	const char* cur_line = src;


	// need to know the end of the current line after EOF or on error
	auto finish_line = [&] () {
		const char* c = cur;
		while (*c != '\n' && *c != '\r' && *c != '\0')
			c++; // skip anything until newline or EOF

		res.lines.emplace_back(strview(cur_line, c - cur_line));
	};
	auto newline = [&] () -> bool {
		const char* before_newline = cur;
		if (parse::newline(cur)) {
			res.lines.emplace_back(strview(cur_line, before_newline - cur_line));
			cur_line = cur;
			return true;
		}
		return false;
	};

	auto throw_error = [&] (const char* errstr, const char* start, const char* cur) {
		finish_line();
		throw Exception{ errstr, start, cur, res.lines.size() };
	};

	using namespace parse;

	for (;;) {
		// skip whitespace
		if (is_whitespace_c(*cur)) {
			cur++;
			continue;
		}

		// if line comment begin, skip until newline or EOF
		if (cur[0] == '/' && cur[1] == '/') {
			cur += 2; // skip "//"

			while (*cur != '\n' && *cur != '\r' && *cur != '\0')
				cur++; // skip anything until newline or EOF

			// fallthrough to newline()
		}
		// if block comment begin, skip until end of block comment while keeping track of nested block comments
		else if (cur[0] == '/' && cur[1] == '*') {
			cur += 2; // skip "/*"

			size_t depth = 1;
			while (depth > 0) {
				if (*cur == '\0') {
					// TODO: also add note about block comment open location to error
					throw_error("end of file in block comment", cur, cur+1);
				}
				else if (newline()) {}
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
			throw_error("unexpected block comment close", cur, cur+2);
		}

		if (newline())
			continue;

		// non-whitespace character outside of comment -> start of a token

		Token& tok = res.tokens.emplace_back();

		tok.lineno = res.lines.size();

		if (*cur == '\0') {
			tok.type = T_EOF;
			tok.text = strview(cur, 1);
			break;
		}

		const char* start = cur;
		switch (*cur) {
			case '+': tok.type = T_PLUS;          break;
			case '-': tok.type = T_MINUS;         break;
			case '*': tok.type = T_MULTIPLY;      break;
			case '/': tok.type = T_DIVIDE;        break;
			case '=': tok.type = T_EQUALS;        break;
			case ';': tok.type = T_SEMICOLON;     break;
			case ',': tok.type = T_COMMA;         break;

			case '(': tok.type = T_PAREN_OPEN;    break; 
			case ')': tok.type = T_PAREN_CLOSE;   break; 
			case '{': tok.type = T_BLOCK_OPEN;    break; 
			case '}': tok.type = T_BLOCK_CLOSE;   break; 
			case '[': tok.type = T_INDEX_OPEN;    break; 
			case ']': tok.type = T_INDEX_CLOSE;   break; 

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {

				char const* start = cur;

				while (is_decimal_c(*cur))
					cur++;

				if (*cur == '.') {
					// float
					cur = start; // reset to begining
					if (!parse_double(cur, &tok.value_flt)) {
						throw_error("number parse error", start, start+1);
					}
					tok.type = T_LITERAL_FLOAT;
					tok.text = strview(start, cur - start);
					continue;
				}
				else {
					// float
					cur = start; // reset to begining
					if (!parse_integer(cur, &tok.value_int)) {
						throw_error("number parse error", start, start+1);
					}
					tok.type = T_LITERAL_INT;
					tok.text = strview(start, cur - start);
					continue;
				}
			}

			case '"': {
				cur++; // skip '"'

				char const* strstart = cur;

				while (*cur != '"') {
					if (*cur == '\0') {
						throw_error("end of file in string literal", cur, cur+1);
					}
					else if (newline()) {}
					else {
						*cur++;
					}
				}

				tok.type = T_LITERAL_STRING;
				tok.value_str = strview(strstart, cur - strstart);

				cur++; // skip '"'

				tok.text = strview(start, cur - start);
				continue;
			}

			default: {
				if (is_ident_start_c(*cur)) {

					while (is_ident_c(*cur))
						cur++; // find end of identifier

					tok.text = strview(start, cur - start);

					if (tok.text  == "for")    tok.type = T_FOR;
					else                       tok.type = T_IDENTIFIER;
					continue;
				}
				else {
					throw_error("unknown token", start, start+1);
				}
			}
		}

		cur++; // single-char token
		tok.text = strview(start, cur - start);
	}

	// after EOF
	finish_line();
	return res;
}
