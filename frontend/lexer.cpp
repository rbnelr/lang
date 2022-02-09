#include "common.hpp"
#include "lexer.hpp"
#include "errors.hpp"

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
constexpr inline bool is_newline_c (char c) {
	return c == '\n' || c == '\r';
}

// ughh... not range-based switch-case...
#define IDENT_START_CASES \
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': \
	case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': \
	case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': \
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': \
	case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': \
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': \
	case '_':

#define NUMBER_START_CASES \
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':


void Lexer::parse_lit_bool    (const char* start, const char* end, Value* out_val) {
	if (start[0] == 'f') {
		assert(strview(start, (size_t)(end - start)) == "false");
		out_val->i = 0;
	}
	else { // if (start[0] == 'f') 
		assert(strview(start, (size_t)(end - start)) == "true");
		out_val->i = 1;
	}
}

void Lexer::parse_lit_integer (const char* start, const char* end, Value* out_val) {
	const char* cur = start;
	assert(*cur >= '0' || *cur <= '9');
	
	int64_t out = 0;
	while ((*cur >= '0' && *cur <= '9')) { //  || *cur == '_'
		//if (*cur != '_') {
			out *= 10;
			out += *cur - '0';
		//}
		cur++;
	}

	if (cur != end)
		SYNTAX_ERROR(get_source_range(start, start+1), "integer parse error");

	out_val->i = (int)out;
}

void Lexer::parse_lit_double  (const char* start, const char* end, Value* out_val) {
	char const* cur = start;
	double val = strtod(start, (char**)&cur); // need to cast away const for strtod api

	if (cur != end)
		SYNTAX_ERROR(get_source_range(start, start+1), "float parse error");
	
	out_val->f = val;
}

void Lexer::parse_lit_string  (const char* start, const char* end, Value* out_val) {
	// resulting strings should be shorter than escaped strings
	size_t max_len = end - start + 1; // +1 to add null terminator

	char* result = g_allocator.alloc_array<char>(max_len);

	const char* in = start;
	char* out = result;

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
					SYNTAX_ERROR(get_source_range(start, in), "invalid escape sequence in literal string");
			}
		} else {
			*out++ = *in++;
		}
	}
	*out++ = '\0';

	size_t real_len = out - result; // real length of generated string
	// don't bother to reallocate the strings just to save a few bytes not needed due to escape sequences
	// g_allocator does not support reallocation currently

	out_val->str = result;
}

TypeClass Lexer::parse_literal (TokenType type, const char* start, const char* end, Value* out_val) {
	switch (type) {
		case T_LITERAL_BOOL:
			parse_lit_bool(start, end, out_val);
			return TY_BOOL;

		case T_LITERAL_INT:
			parse_lit_integer(start, end, out_val);
			return TY_INT;

		case T_LITERAL_FLT:
			parse_lit_double(start, end, out_val);
			return TY_FLT;

		case T_LITERAL_STR:
			parse_lit_string(start, end, out_val);
			return TY_STR;

		INVALID_DEFAULT;
	}
	return (TypeClass)0;
}

#include <algorithm>

struct _Keyword {
	strview   str;
	TokenType tok;
};

struct KeywordsByLen {
	
	static _FORCEINLINE uint64_t read_u64_safe (char const* p, int len) {
		if (len == 8)
			return *(uint64_t*)p;
		
		char const* end = p + len;
		int i = 0;

		uint64_t val = 0;
		if (p <= end-4) {
			val |= (uint64_t)(*(uint32_t*)p) << i;
			p += 4; i += 32;
		}
		if (p <= end-2) {
			val |= (uint64_t)(*(uint16_t*)p) << i;
			p += 2; i += 16;
		}
		if (p <= end-1) {
			val |= (uint64_t)(*(uint8_t*)p) << i;
			p += 1; i += 8;
		}
		return val;
	}
	// depends on little vs. big-endian
	static constexpr uint64_t KW (char const* p) {
		int val = 0;
		for (int i=0; *p && i<8; ++i)
			val |= ((uint64_t)*p++) << (8*i);
		return val;
	}

	_NOINLINE TokenType get (const char* str, size_t len) {
		static_assert(KW("printf") == 0x66746E697270ull, "");
		
		if (len > 1 && len <= 8) {
			uint64_t stri = read_u64_safe(str, (int)len);
			switch (stri) {
				case KW("func"     ): return T_FUNC;
				case KW("struct"   ): return T_STRUCT;

				case KW("if"       ): return T_IF;
				case KW("elif"     ): return T_ELIF;
				case KW("else"     ): return T_ELSE;
					
				case KW("while"    ): return T_WHILE;
				case KW("for"      ): return T_FOR;
				case KW("do"       ): return T_DO;

				case KW("return"   ): return T_RETURN;
				case KW("break"    ): return T_BREAK;
				case KW("continue" ): return T_CONTINUE;
				case KW("goto"     ): return T_GOTO;

				case KW("true"     ):
				case KW("false"    ): return T_LITERAL_BOOL;
			}
		}
		return T_IDENTIFIER;
	}
};
KeywordsByLen keywords_by_len;

std::unordered_map<strview, TokenType> keywords_hashed = {
	{ {"if"       }, T_IF           },
	{ {"elif"     }, T_ELIF         },
	{ {"else"     }, T_ELSE         },
	{ {"while"    }, T_WHILE        },
	{ {"for"      }, T_FOR          },
	{ {"do"       }, T_DO           },
	
	{ {"true"     }, T_LITERAL_BOOL },
	{ {"false"    }, T_LITERAL_BOOL },
	
	{ {"func"     }, T_FUNC         },
	{ {"struct"   }, T_STRUCT       },
	
	{ {"return"   }, T_RETURN       },
	{ {"break"    }, T_BREAK        },
	{ {"continue" }, T_CONTINUE     },
	{ {"goto"     }, T_GOTO         },
};

_FORCEINLINE TokenType match_keyword (char const* start, char const* end) {
#if 0
	auto text = std::string_view(start, (size_t)(end - start));

	if (text == "if"       ) return T_IF;         
	if (text == "elif"     ) return T_ELIF;       
	if (text == "else"     ) return T_ELSE;       
	if (text == "while"    ) return T_WHILE;      
	if (text == "for"      ) return T_FOR;        
	if (text == "do"       ) return T_DO;         

	//if (text == "null"     ) return T_LITERAL;
	if (text == "true"     ) return T_LITERAL_BOOL;
	if (text == "false"    ) return T_LITERAL_BOOL;

	if (text == "func"     ) return T_FUNC;        
	if (text == "struct"   ) return T_STRUCT;      

	if (text == "return"   ) return T_RETURN;      
	if (text == "break"    ) return T_BREAK;       
	if (text == "continue" ) return T_CONTINUE;    
	if (text == "goto"     ) return T_GOTO;        

	return T_IDENTIFIER;
#elif 1
	auto text = std::string_view(start, (size_t)(end - start));
	return keywords_by_len.get(text.data(), text.size());
#elif 0
	auto text = std::string_view(start, (size_t)(end - start));
	auto it = keywords_hashed.find(text);
	if (it == keywords_hashed.end())
		return T_IDENTIFIER;
	return it->second;
#else
	
#endif
}

void Lexer::lex (Token* first_tok, Token* end_tok) {
	const char* cur = cur_char; // copy into local to help compiler avoid reloading this during the loop

	Token* out_tok = first_tok;

	auto newline = [&] () {
		assert(*cur == '\n' || *cur == '\r');

		// newline found
		char c = *cur++;

		// this code should even handle files with inconsistent unix vs windows newlines reasonably
		// "\n" "\r" "\n\r" "\r\n" each count as one newline while "\n\n" "\r\r" count as two
		if ((*cur == '\n' || *cur == '\r') && c != *cur)
			cur++;

		cur_line = cur;
		cur_lineno++;
	};

	for (;;) {
		switch (*cur) {
			// skip whitespace
			case '\n': case '\r': {
				newline();
				continue;
			}
			case ' ': case '\t': {
				cur++;
				continue;
			}
			
			case '/': { // '//' or '/*'
				// "//" if line comment begin, skip until newline or EOF
				if (cur[1] == '/') {
					cur+=2;

					while (!is_newline_c(*cur) && *cur != '\0')
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
							SYNTAX_ERROR(get_source_range(cur, cur+1), "end of file in block comment");
						}
						else if (cur[0] == '/' && cur[1] == '*') {
							cur += 2; // skip "/*"
							depth++;
						}
						else if (cur[0] == '*' && cur[1] == '/') {
							cur += 2; // skip "*/"
							depth--;
						}
						else if (is_newline_c(*cur)) {
							newline();
						}
						else {
							cur++;
						}
					}
					continue;
				}
			} break;
			case '*': { // '*/'
				if (cur[1] == '/') {
					SYNTAX_ERROR(get_source_range(cur, cur+2), "unexpected block comment close");
				}
			} break;
		}

		// non-whitespace character outside of comment -> start of a token

		if (out_tok >= end_tok) {
			cur_char = cur;
			break; // end lexing loop
		}
		
		const char* start = cur;

		Token& tok = *out_tok++;

		set_source_range_start(&tok.source, start);
		
		// tok.source.length = LEN: avoid range check by not calling set_source_range_len()
		#define SIMPLE_TOK(TYPE, LEN) {             \
			tok.type = TYPE;                        \
			tok.source.length = (uint16_t)LEN;      \
			cur += LEN;                             \
			continue;                               \
		}
		
		switch (*cur) {

			case '\0': {
				tok.type = T_EOF;
				tok.source.length = 1;

				// break would exit switch
				// and we really want this to be in the switch to remove, since this removes one conditional from every token lexing)
				goto L_exit; // end lexing loop
			}

			case '+':
				if (cur[1] == '=')      SIMPLE_TOK(T_ADDEQ, 2)
				else if (cur[1] == '+') SIMPLE_TOK(T_INC,   2)
				else                    SIMPLE_TOK(T_ADD,   1)

			case '-':
				if (cur[1] == '=')      SIMPLE_TOK(T_SUBEQ, 2)
				else if (cur[1] == '-') SIMPLE_TOK(T_DEC,   2)
				else                    SIMPLE_TOK(T_SUB,   1)

			case '*':
				if (cur[1] == '=') SIMPLE_TOK(T_MULEQ,      2)
				else               SIMPLE_TOK(T_MUL,        1)

			case '/':
				if (cur[1] == '=') SIMPLE_TOK(T_DIVEQ,      2)
				else               SIMPLE_TOK(T_DIV,        1)

			case '%':
				if (cur[1] == '=') SIMPLE_TOK(T_MODEQ,      2)
				else               SIMPLE_TOK(T_MOD,        1)
				
			case '&':
				if (cur[1] == '&') SIMPLE_TOK(T_AND,        2)
				else               SIMPLE_TOK(T_BIT_AND,    1)

			case '|':
				if (cur[1] == '|') SIMPLE_TOK(T_OR,         2)
				else               SIMPLE_TOK(T_BIT_OR,     1)

			case '<':
				if (cur[1] == '=') SIMPLE_TOK(T_LESSEQ,     2)
				else               SIMPLE_TOK(T_LESS,       1)

			case '>':
				if (cur[1] == '=') SIMPLE_TOK(T_GREATEREQ,  2)
				else               SIMPLE_TOK(T_GREATER,    1)

			case '!':
				if (cur[1] == '=') SIMPLE_TOK(T_NOT_EQUALS, 2)
				else               SIMPLE_TOK(T_NOT,        1)

			case '=':
				if (cur[1] == '=') SIMPLE_TOK(T_EQUALS,     2)
				else               SIMPLE_TOK(T_ASSIGN,     1)
				
			case '~':      SIMPLE_TOK(T_BIT_NOT,         1)
			case '^':      SIMPLE_TOK(T_BIT_XOR,         1)
			
			case '.':      SIMPLE_TOK(T_MEMBER,          1)
			case ':':      SIMPLE_TOK(T_COLON,           1)
			case ';':      SIMPLE_TOK(T_SEMICOLON,       1)
			case ',':      SIMPLE_TOK(T_COMMA,           1)
			case '?':      SIMPLE_TOK(T_QUESTIONMARK,    1)
			
			case '(':      SIMPLE_TOK(T_PAREN_OPEN,      1)
			case ')':      SIMPLE_TOK(T_PAREN_CLOSE,     1)
			case '{':      SIMPLE_TOK(T_BLOCK_OPEN,      1)
			case '}':      SIMPLE_TOK(T_BLOCK_CLOSE,     1)
			case '[':      SIMPLE_TOK(T_INDEX_OPEN,      1)
			case ']':      SIMPLE_TOK(T_INDEX_CLOSE,     1)
			
			IDENT_START_CASES {
				while (is_ident_c(*cur))
					cur++; // find end of identifier

				set_source_range_len(&tok.source, cur - start);

				tok.type = match_keyword(start, cur);
				continue;
			}

			NUMBER_START_CASES {

				while (is_decimal_c(*cur))
					cur++;

				// float
				if (*cur == '.') {

					cur++;
					while (is_decimal_c(*cur))
						cur++;

					tok.type = T_LITERAL_FLT;
				}
				// int
				else {
					tok.type = T_LITERAL_INT;
				}
				set_source_range_len(&tok.source, cur - start);
				continue;
			}

			case '"': {
				cur++; // skip '"'

				char const* strstart = cur;

				for (;;) {
					if (*cur == '\0') {
						SYNTAX_ERROR(get_source_range(cur, cur+1), "end of file in string literal");
					}
					else if (is_newline_c(*cur)) {
						//SYNTAX_ERROR(get_source_range(cur, cur+1), "newline in string literal"); // Allow newlines?
						newline();
					}
					// escape sequences \\ and \"
					else if (cur[0] == '\\' && (cur[1] == '"' || cur[1] == '\\')) {
						cur += 2;
					}
					else if (*cur == '"') {
						break;
					}
					cur++;
				}

				char const* strend = cur++; // skip '"'

				tok.type = T_LITERAL_STR;
				set_source_range_len(&tok.source, cur - start);
				continue;
			}

			default: {
				SYNTAX_ERROR(tok.source, "unknown token");
			}
		}
	}
	L_exit:

	cur_char = cur;
}

#undef SIMPLE_TOK
