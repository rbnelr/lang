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

void match_keyword (char const* start, char const* end, Token& tok) {
#if 0
	auto text = tok.source.text();

	struct Keyword {
		char const* str;
		Token       val;


	};
	std::vector<std::vector<char const*>>

	switch (text.size()) {
		case 2: {
			if      (text == "if"   ) { tok.type = T_IF;                           continue; }
			else if (text == "do"   ) { tok.type = T_DO;                           continue; }
		} break;
		case 3: {
			if      (text == "for"  ) { tok.type = T_FOR;                          continue; }
		} break;
		case 4: {
			if      (text == "elif" ) { tok.type = T_ELIF;                         continue; }
			else if (text == "else" ) { tok.type = T_ELSE;                         continue; }
			else if (text == "true" ) { tok.type = T_LITERAL; tok.lit_type = TY_BOOL; tok.lit_val.b = true; continue; }
			else if (text == "func" ) { tok.type = T_FUNC;  continue; }
			else if (text == "goto" ) { tok.type = T_GOTO;  continue; }
		} break;
		case 5: {
			if      (text == "while") { tok.type = T_LITERAL; tok.val = { false }; continue; }
		} break;
	}
	tok.type = T_IDENTIFIER;
#else
	auto text = std::string_view(start, (size_t)(end - start));

	if      (text == "if"       ) tok.type = T_IF;         
	else if (text == "elif"     ) tok.type = T_ELIF;       
	else if (text == "else"     ) tok.type = T_ELSE;       
	else if (text == "while"    ) tok.type = T_WHILE;      
	else if (text == "for"      ) tok.type = T_FOR;        
	else if (text == "do"       ) tok.type = T_DO;         

	//else if (text == "null"     ) { tok.type = T_LITERAL;
	else if (text == "true"     ) tok.type = T_LITERAL_BOOL;
	else if (text == "false"    ) tok.type = T_LITERAL_BOOL;

	else if (text == "func"     ) tok.type = T_FUNC;        
	else if (text == "struct"   ) tok.type = T_STRUCT;      

	else if (text == "return"   ) tok.type = T_RETURN;      
	else if (text == "break"    ) tok.type = T_BREAK;       
	else if (text == "continue" ) tok.type = T_CONTINUE;    
	else if (text == "goto"     ) tok.type = T_GOTO;        

	else tok.type = T_IDENTIFIER;
#endif
}

struct _LUT {
	struct LUT_Entry {
		TokenType a = (TokenType)0;

		char      b0c = -1;
		TokenType b0t = (TokenType)0;

		char      b1c = -1;
		TokenType b1t = (TokenType)0;
	};
	LUT_Entry LUT[128];
};
constexpr _LUT _gen_LUT () {
	_LUT l = {};

	l.LUT['~'] = { T_BIT_NOT      };
	l.LUT['^'] = { T_BIT_XOR      };
	
	l.LUT['.'] = { T_MEMBER       };
	l.LUT[':'] = { T_COLON        };
	l.LUT[';'] = { T_SEMICOLON    };
	l.LUT[','] = { T_COMMA        };
	l.LUT['?'] = { T_QUESTIONMARK };
	
	l.LUT['('] = { T_PAREN_OPEN   };
	l.LUT[')'] = { T_PAREN_CLOSE  };
	l.LUT['{'] = { T_BLOCK_OPEN   };
	l.LUT['}'] = { T_BLOCK_CLOSE  };
	l.LUT['['] = { T_INDEX_OPEN   };
	l.LUT[']'] = { T_INDEX_CLOSE  };

	
	l.LUT['*'] = { T_MUL,         '=', T_MULEQ      };
	l.LUT['/'] = { T_DIV,         '=', T_DIVEQ      };
	l.LUT['%'] = { T_MOD,         '=', T_MODEQ      };
	l.LUT['&'] = { T_BIT_AND,     '&', T_AND        };
	l.LUT['|'] = { T_BIT_OR,      '|', T_OR         };
	l.LUT['<'] = { T_LESS,        '=', T_LESSEQ     };
	l.LUT['>'] = { T_GREATER,     '=', T_GREATEREQ  };
	l.LUT['!'] = { T_NOT,         '=', T_NOT_EQUALS };
	l.LUT['='] = { T_ASSIGN,      '=', T_EQUALS     };

	l.LUT['!'] = { T_NOT,         '=', T_NOT_EQUALS };
	l.LUT['='] = { T_ASSIGN,      '=', T_EQUALS     };
	

	l.LUT['+'] = { T_ADD,         '=', T_ADDEQ,     '+', T_INC};
	l.LUT['-'] = { T_SUB,         '=', T_SUBEQ,     '-', T_DEC};
	
	return l;
}
constexpr _LUT LUT = _gen_LUT(); 

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
		
		if (*cur == '\0') {
			tok.type = T_EOF;
			tok.source.length = 1;
			break; // end lexing loop
		}
		
		#define SIMPLE_TOK(TYPE, LEN) {             \
			cur += LEN;                             \
			tok.type = TYPE;                        \
			set_source_range_len(&tok.source, LEN); \
			continue;                               \
		}

		if (is_ident_start_c(*cur)) {

			while (is_ident_c(*cur))
				cur++; // find end of identifier

			set_source_range_len(&tok.source, cur - start);

			match_keyword(start, cur, tok);
			continue;
		}
		
		auto& l = LUT.LUT[*cur];
		if (l.a != T_EOF) {
			cur++;
			auto t = l.a;
			int len = 1;

			char bc = *cur;
			if      (bc == l.b0c) { t = l.b0t; cur++; }
			else if (bc == l.b1c) { t = l.b1t; cur++; }

			tok.type = t;
			set_source_range_len(&tok.source, cur - start);
			continue;
		}
		
		if (*cur >= '0' && *cur <= '9') {
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
		
		if (*cur == '"') {
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

		SYNTAX_ERROR(tok.source, "unknown token");
	}

	cur_char = cur;
}

#undef SIMPLE_TOK
