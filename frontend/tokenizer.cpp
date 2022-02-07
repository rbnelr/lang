#include "common.hpp"
#include "tokenizer.hpp"
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

const char* parse_escaped_string (const char* start, const char* end) {
	// resulting strings should be shorter than escaped strings
	size_t max_len = end - start + 1;

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
					SYNTAX_ERROR(source_range(start, in), "invalid escape sequence in literal string");
			}
		} else {
			*out++ = *in++;
		}
	}
	*out++ = '\0';

	size_t real_len = out - result; // real length of generated string
	// don't bother to reallocate the strings just to save a few bytes not needed due to escape sequences
	// g_allocator does not support reallocation currently

	return result;
}

const char* tokenize (Token* buf, Token* bufend, const char* cur_src, SourceLines& lines) {
	const char* cur = cur_src;

	Token* cur_tok = buf;

	auto newline = [&] () {
		assert(*cur == '\n' || *cur == '\r');

		// newline found
		char c = *cur++;

		// this code should even handle files with inconsistent unix vs windows newlines reasonably
		// "\n" "\r" "\n\r" "\r\n" each count as one newline while "\n\n" "\r\r" count as two
		if ((*cur == '\n' || *cur == '\r') && c != *cur)
			cur++;

		// add next line
		lines.lines.emplace_back(cur);
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
							SYNTAX_ERROR(source_range(cur, cur+1), "end of file in block comment");
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
					SYNTAX_ERROR(source_range(cur, cur+2), "unexpected block comment close");
				}
			} break;
		}

		// non-whitespace character outside of comment -> start of a token

		if (cur_tok >= bufend)
			return cur;

		Token& tok = *cur_tok++;

		if (*cur == '\0') {
			tok.type = T_EOF;
			tok.source = { cur, cur+1 };
			
			// add dummy line after EOF on one past the EOF character
			// TODO: is this safe? I changed this from the dummy line being _on_ the EOF
			// since my binary search below had a bug in the case of the source loc being _on_ the EOF as well
			lines.lines.emplace_back(cur + 1);
			
			return cur;
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
				else {             tok.type = T_MULEQ;       cur++; }
				break;

			case '/':
				if (cur[1] != '=') tok.type = T_DIV;
				else {             tok.type = T_DIVEQ;       cur++; }
				break;

			case '%':
				if (cur[1] != '=') tok.type = T_MOD;
				else {             tok.type = T_MODEQ;       cur++; }
				break;
				
			case '&':
				if (cur[1] != '&') tok.type = T_BIT_AND;
				else {             tok.type = T_AND;         cur++; }
				break;

			case '|':
				if (cur[1] != '|') tok.type = T_BIT_OR;
				else {             tok.type = T_OR;          cur++; }
				break;

			case '<':
				if (cur[1] != '=') tok.type = T_LESS;
				else {             tok.type = T_LESSEQ;      cur++; }
				break;

			case '>':
				if (cur[1] != '=') tok.type = T_GREATER;
				else {             tok.type = T_GREATEREQ;   cur++; }
				break;

			case '!':
				if (cur[1] != '=') tok.type = T_NOT;
				else {             tok.type = T_NOT_EQUALS;  cur++; }
				break;

			case '=':
				if (cur[1] != '=') tok.type = T_ASSIGN;
				else {             tok.type = T_EQUALS;      cur++; }
				break;
				
			case '~': tok.type = T_BIT_NOT;       break;
			case '^': tok.type = T_BIT_XOR;       break;

			case '.': tok.type = T_MEMBER;        break;
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
						SYNTAX_ERROR(source_range(start, start+1), "number parse error");
					}
					tok.type = T_LITERAL;
					tok.source = { start, cur };
					tok.lit_type = TY_FLT;
					tok.lit_val.f = val;
					continue;
				}
				// int
				else {

					cur = start; // reset to begining
					int64_t val;
					if (!parse_integer(cur, &val))
						SYNTAX_ERROR(source_range(start, start+1), "number parse error");
					
					tok.type = T_LITERAL;
					tok.source = { start, cur };
					tok.lit_type = TY_INT;
					tok.lit_val.i = val;
					continue;
				}
			}

			case '"': {
				cur++; // skip '"'

				char const* strstart = cur;

				for (;;) {
					if (*cur == '\0') {
						SYNTAX_ERROR(source_range(cur, cur+1), "end of file in string literal");
					}
					else if (is_newline_c(*cur)) {
						SYNTAX_ERROR(source_range(cur, cur+1), "newline in string literal"); // Allow newlines?
						//newline();
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

				tok.type = T_LITERAL;
				tok.source = { start, cur };
				tok.lit_type = TY_STR;
				tok.lit_val.str = parse_escaped_string(strstart, strend); // TODO: scanning this string twice, does this need to happen?
				continue;
			}

			default: {
				if (is_ident_start_c(*cur)) {

					while (is_ident_c(*cur))
						cur++; // find end of identifier

					tok.source = { start, cur };

				#if 0
					auto text = tok.tok.text();

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
					if      (text == "if"       ) { tok.type = T_IF;                           }
					else if (text == "elif"     ) { tok.type = T_ELIF;                         }
					else if (text == "else"     ) { tok.type = T_ELSE;                         }
					else if (text == "while"    ) { tok.type = T_WHILE;                        }
					else if (text == "for"      ) { tok.type = T_FOR;                          }
					else if (text == "do"       ) { tok.type = T_DO;                           }

					//else if (text == "null"     ) { tok.type = T_LITERAL; tok.lit_type = BOOL; tok.lit_val.b = {};        }
					else if (text == "true"     ) { tok.type = T_LITERAL; tok.lit_type = TY_BOOL; tok.lit_val.b = true;  }
					else if (text == "false"    ) { tok.type = T_LITERAL; tok.lit_type = TY_BOOL; tok.lit_val.b = false; }

					else if (text == "func"     ) { tok.type = T_FUNC;                         }
					else if (text == "struct"   ) { tok.type = T_STRUCT;                       }

					else if (text == "return"   ) { tok.type = T_RETURN;                       }
					else if (text == "break"    ) { tok.type = T_BREAK;                        }
					else if (text == "continue" ) { tok.type = T_CONTINUE;                     }
					else if (text == "goto"     ) { tok.type = T_GOTO;                         }

					else {
						tok.type = T_IDENTIFIER;
					}
					continue;
				#endif
				}

				SYNTAX_ERROR(source_range(start, start+1), "unknown token");
			}
		}

		cur++; // single-char token
		tok.source = { start, cur };
	}
}
