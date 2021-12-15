#pragma once
#include "common.hpp"
#include "errors.hpp"
#include "types.hpp"
#include "line_map.hpp"
#include "ident_ids.hpp"

constexpr inline bool is_decimal_c (char c) {
	return c >= '0' && c <= '9';
}
constexpr inline bool is_hex_c (char c) {
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
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

// parse 1_000_000 as 1000000 for better readability
// (user can put _ anywhere in int after intial digit
inline bool parse_integer (const char*& c, int64_t* out_int) {
	const char* cur = c;
	if (*cur < '0' || *cur > '9')
		return false;

	int64_t out = 0;
	while ((*cur >= '0' && *cur <= '9') || *cur == '_') {
		if (*cur != '_') {
			out *= 10;
			out += *cur - '0';
		}
		cur++;
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

enum TokenType {
	T_EOF=0, // end of file

	T_ADD,           // +
	T_SUB,           // -   binary operator   OR   unary (prefix operator)
	T_MUL,           // *
	T_DIV,           // /

	T_LESS,          // <
	T_LESSEQ,        // <=
	T_GREATER,       // >
	T_GREATEREQ,     // >=
	T_EQUALS,        // ==
	T_NOT_EQUALS,    // !=

	T_QUESTIONMARK,  // ?

	T_NOT,           // !   unary (prefix) operator
	T_INC,           // x++  postincrement
	T_DEC,           // x--  postdecrement

	T_ASSIGN,        // =
	T_ADDEQ,         // +=
	T_SUBEQ,         // -=
	T_MULEQ,         // *=
	T_DIVEQ,         // /=

	T_COLON,         // :
	T_SEMICOLON,     // ;
	T_COMMA,         // ,

	T_PAREN_OPEN,    // (
	T_PAREN_CLOSE,   // )
	T_BLOCK_OPEN,    // {
	T_BLOCK_CLOSE,   // }
	T_INDEX_OPEN,    // [
	T_INDEX_CLOSE,   // ]

	// literals of differing types
	T_LITERAL,

	// starts with  '_' or [a-Z]  and then any number of  '_' or [a-Z] or [0-9]
	T_IDENTIFIER,

	// keywords
	T_IF,
	T_ELIF,
	T_ELSE,
	T_FOR,
};
inline constexpr const char* TokenType_str[] = {
	"T_EOF",

	"T_ADD",
	"T_SUB",
	"T_MUL",
	"T_DIV",

	"T_LESS",
	"T_LESSEQ",
	"T_GREATER",
	"T_GREATEREQ",
	"T_EQUALS",
	"T_NOT_EQUALS",

	"T_QUESTIONMARK",

	"T_NOT",
	"T_INC",
	"T_DEC",

	"T_ASSIGN",
	"T_ADDEQ",
	"T_SUBEQ",
	"T_MULEQ",
	"T_DIVEQ",

	"T_COLON",
	"T_SEMICOLON",
	"T_COMMA",

	"T_PAREN_OPEN",
	"T_PAREN_CLOSE",
	"T_BLOCK_OPEN",
	"T_BLOCK_CLOSE",
	"T_INDEX_OPEN",
	"T_INDEX_CLOSE",
	
	"T_LITERAL",
	
	"T_IDENTIFIER",

	"T_IF",
	"T_ELIF",
	"T_ELSE",
	"T_FOR",
};
/*
inline constexpr const char* TokenType_char[] = {
	"\\0",

	"+",
	"-",
	"*",
	"/",

	"<",
	"<=",
	">",
	">=",
	"==",
	"!=",

	"?",

	"!",
	"++",
	"--",

	"=",
	"+=",
	"-=",
	"*=",
	"/=",

	":",
	";",
	",",
	
	"(",
	")",
	"{",
	"}",
	"[",
	"]",

	"literal",

	"identifier",

	"if",
	"elif",
	"else",
	"for",
};
*/

struct Token {
	TokenType    type;
	source_range source;

	Value        val;
};

Value parse_escaped_string (const char* start, const char* end) {
	Value val;
	val.type = STR;

	// resulting strings should be shorter than escaped strings
	val.u.str = (char*)malloc(end - start + 1);

	const char* in = start;
	char*       out = val.u.str;

	while (in < end) {
		if (*in == '\\') {
			auto start = in++;
			switch (*in++) {
				case '0' : *out++ = '\0'; break;
				case 'n' : *out++ = '\n'; break;
				case 'r' : *out++ = '\r'; break;
				case '\\': *out++ = '\\'; break;
				case '"' : *out++ = '\"'; break;
				default:
					throw MyException{ "invalid escape sequence in literal string", start, in };
			}
		} else {
			*out++ = *in++;
		}
	}
	*out++ = '\0';

	val.u.str = (char*)realloc(val.u.str, out - val.u.str); // resize to (smaller) real size
	return val;
}

std::vector<Token> tokenize (const char* src, IdentiferIDs& ident_ids) {
	ZoneScoped;
	std::vector<Token> tokens;
	tokens.reserve(1024*8);

	const char* cur = src;
	const char* cur_line = src;

	for (;;) {
		switch (*cur) {
			// skip whitespace
			case ' ': case '\t':
			case '\n': case '\r': {
				cur++;
				continue;
			}
			
			case '/': {
				// "//" if line comment begin, skip until newline or EOF
				if (cur[1] == '/') {
					cur+=2;

					while (*cur != '\n' && *cur != '\r' && *cur != '\0')
						cur++; // skip anything until newline or EOF

					continue;
				}
				// "/*" if block comment begin, skip until end of block comment while keeping track of nested block comments
				else if (cur[1] == '*') {
					cur+=2;

					size_t depth = 1;
					while (depth > 0) {
						if (*cur == '\0') {
							// TODO: also add note about block comment open location to error
							throw MyException{ "end of file in block comment", {cur, cur+1}};
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
			} break;
			case '*': {
				if (cur[1] == '/') {
					throw MyException{"unexpected block comment close", {cur, cur+2}};
				}
			} break;
		}

		// non-whitespace character outside of comment -> start of a token

		Token& tok = tokens.emplace_back();

		if (*cur == '\0') {
			tok.type = T_EOF;
			tok.source = { cur, cur+1 };
			break;
		}

		const char* start = cur;
		switch (*cur) {
			case '+':
				if (cur[1] == '=')      { tok.type = T_ADDEQ; cur++; }
				else if (cur[1] == '+') { tok.type = T_INC;   cur++; }
				else                    { tok.type = T_ADD; }
				break;

			case '-':
				if (cur[1] == '=')      { tok.type = T_SUBEQ; cur++; }
				else if (cur[1] == '-') { tok.type = T_DEC;   cur++; }
				else                    { tok.type = T_SUB; }
				break;

			case '*':
				if (cur[1] != '=') tok.type = T_MUL;
				else {             tok.type = T_MULEQ;      cur++; }
				break;

			case '/':
				if (cur[1] != '=') tok.type = T_DIV;
				else {             tok.type = T_DIVEQ;      cur++; }
				break;

			case '<':
				if (cur[1] != '=') tok.type = T_LESS;
				else {             tok.type = T_LESSEQ;     cur++; }
				break;

			case '>':
				if (cur[1] != '=') tok.type = T_GREATER;
				else {             tok.type = T_GREATEREQ;  cur++; }
				break;

			case '!':
				if (cur[1] != '=') tok.type = T_NOT;
				else {             tok.type = T_NOT_EQUALS; cur++; }
				break;

			case '=':
				if (cur[1] != '=') tok.type = T_ASSIGN;
				else {             tok.type = T_EQUALS;     cur++; }
				break;

			case ':': tok.type = T_COLON;         break;
			case ';': tok.type = T_SEMICOLON;     break;
			case ',': tok.type = T_COMMA;         break;
			case '?': tok.type = T_QUESTIONMARK;  break;

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
				
				// float
				if (*cur == '.') {
					cur = start; // reset to begining
					double val;
					if (!parse_double(cur, &val)) {
						throw MyException{"number parse error", {start, start+1}};
					}
					tok.type = T_LITERAL;
					tok.source = { start, cur };
					tok.val = val;
					continue;
				}
				// int
				else {

					cur = start; // reset to begining
					int64_t val;
					if (!parse_integer(cur, &val))
						throw MyException{"number parse error", {start, start+1}};
					
					tok.type = T_LITERAL;
					tok.source = { start, cur };
					tok.val = val;
					continue;
				}
			}

			case '"': {
				cur++; // skip '"'

				char const* strstart = cur;

				for (;;) {
					if (*cur == '\0')
						throw MyException{"end of file in string literal", {cur, cur+1}};
					// escape sequences \\ and \"
					else if (cur[0] == '\\' && (cur[1] == '"' || cur[1] == '\\'))
						cur += 2;
					else if (*cur == '"')
						break;
					cur++;
				}

				char const* strend = cur++; // skip '"'

				tok.type = T_LITERAL;
				tok.source = { start, cur };
				tok.val = parse_escaped_string(strstart, strend); // TODO: scanning this string twice, does this need to happen?
				continue;
			}

			default: {
				if (is_ident_start_c(*cur)) {

					while (is_ident_c(*cur))
						cur++; // find end of identifier

					tok.source = { start, cur };

				#if 0
					auto text = tok.source.text();

					switch (text.size()) {
						case 2: {
							if      (text == "if"   ) { tok.type = T_IF;                           continue; }
						} break;
						case 3: {
							if      (text == "for"  ) { tok.type = T_FOR;                          continue; }
						} break;
						case 4: {
							if      (text == "elif" ) { tok.type = T_ELIF;                         continue; }
							else if (text == "else" ) { tok.type = T_ELSE;                         continue; }
							else if (text == "null" ) { tok.type = T_LITERAL; tok.val = {};        continue; }
							else if (text == "true" ) { tok.type = T_LITERAL; tok.val = { true };  continue; }
						} break;
						case 5: {
							if      (text == "false") { tok.type = T_LITERAL; tok.val = { false }; continue; }
						} break;
					}
					tok.type = T_IDENTIFIER;
					continue;
				#else
					auto text = tok.source.text();
					if      (text == "if"   )   tok.type = T_IF;
					else if (text == "elif" )   tok.type = T_ELIF;
					else if (text == "else" )   tok.type = T_ELSE;
					else if (text == "for"  )   tok.type = T_FOR;
					else if (text == "null" ) { tok.type = T_LITERAL; tok.val = {}; }
					else if (text == "true" ) { tok.type = T_LITERAL; tok.val = { true };  }
					else if (text == "false") { tok.type = T_LITERAL; tok.val = { false }; }
					else {
						tok.type = T_IDENTIFIER;
					}
					continue;
				#endif
				}

				throw MyException{"unknown token", {start, start+1}};
			}
		}

		cur++; // single-char token
		tok.source = { start, cur };
	}
	return tokens;
}
