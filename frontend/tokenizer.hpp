#pragma once
#include "common.hpp"
#include "basic_types.hpp"
#include "line_map.hpp"

#define TOKTYPES \
	/*end of file*/                    \
	X( EOF=0,         "<EOF>" )        \
	/* literals of differing types */  \
	X( LITERAL,       "literal" )      \
	/* starts with  '_' or [a-Z]  and then any number of  '_' or [a-Z] or [0-9] */ \
	X( IDENTIFIER,    "identifier" ) \
\
	X( FUNC,          "func"       ) \
	X( STRUCT,        "struct"     ) \
	/* keywords */ \
	X( IF,            "if"         ) \
	X( ELIF,          "elif"       ) \
	X( ELSE,          "else"       ) \
\
	X( WHILE,         "while"      ) \
	X( FOR,           "for"        ) \
	X( DO,            "do"         ) \
\
	X( RETURN,        "return"     ) \
	X( BREAK,         "break"      ) \
	X( CONTINUE,      "continue"   ) \
	X( GOTO,          "goto"       ) \
\
	X( COLON,         ":"  ) \
	X( SEMICOLON,     ";"  ) \
	X( COMMA,         ","  ) \
\
	X( PAREN_OPEN,    "("  ) \
	X( PAREN_CLOSE,   ")"  ) \
	X( BLOCK_OPEN,    "{"  ) \
	X( BLOCK_CLOSE,   "}"  ) \
	X( INDEX_OPEN,    "["  ) \
	X( INDEX_CLOSE,   "]"  ) \
\
	X( ADD,           "+"  ) \
	X( SUB,           "-"  ) \
	X( MUL,           "*"  ) \
	X( DIV,           "/"  ) \
	X( MOD,           "%"  ) \
\
	X( BIT_AND,       "&"  ) \
	X( BIT_OR,        "|"  ) \
	X( BIT_XOR,       "^"  ) \
\
	X( AND,           "&&" ) \
	X( OR,            "||" ) \
\
	X( LESS,          "<"  ) \
	X( LESSEQ,        "<=" ) \
	X( GREATER,       ">"  ) \
	X( GREATEREQ,     ">=" ) \
	X( EQUALS,        "==" ) \
	X( NOT_EQUALS,    "!=" ) \
\
	X( MEMBER,        "."  ) \
\
	X( QUESTIONMARK,  "?"  ) \
\
	X( BIT_NOT,       "~"   ) \
	X( NOT,           "!"   ) /* unary (prefix) operator */ \
	X( INC,           "x++" ) /* postincrement */ \
	X( DEC,           "x--" ) /* postdecrement */ \
\
	X( ASSIGN,        "="   ) \
	X( ADDEQ,         "+="  ) \
	X( SUBEQ,         "-="  ) \
	X( MULEQ,         "*="  ) \
	X( DIVEQ,         "/="  ) \
	X( MODEQ,         "%="  ) \

#define X(ENUM, SHORTSTR) T_##ENUM,
enum TokenType {
	TOKTYPES
};
#undef X

#define X(ENUM, SHORTSTR) STRINGIFY(T_##ENUM),
inline constexpr const char* TokenType_str[] = {
	TOKTYPES
};
#undef X

#define X(ENUM, SHORTSTR) SHORTSTR,
inline constexpr const char* TokenType_char[] = {
	TOKTYPES
};
#undef X
#undef TOKTYPES


struct Token {
	TokenType    type;
	source_range source;

	TypeClass    lit_type;
	Value        lit_val;
};

const char* parse_escaped_string (const char* start, const char* end);

const char* tokenize (Token* buf, Token* bufend, const char* cur_src, SourceLines& lines);

inline int tokenize_count;

struct Tokenizer {
	const char* cur_src;
	Token*      cur_tok;

	SourceLines lines;
	
	static inline constexpr int LOOKBACK = 1;
	// how many tokes are always valid relative to lookahead with peek(), eg. peek(LOOKAHEAD-1) is safe
	static inline constexpr int LOOKAHEAD = 2;

	static inline constexpr int WINDOW_SIZE = LOOKBACK + LOOKAHEAD; // how many buffered tokens are valid to access at any one point
	static inline constexpr int KEEP_TOKENS = WINDOW_SIZE -1; // how many tokens are kept in refill_buf() when reaching the end of the buffer

	//static inline constexpr int BUFSZ = 256; // if  sizeof(Token) * BUFSZ  fits into a cpu cache level that should improve perf 
	
	//Token buf[BUFSZ];
	Token* buf = nullptr;
	int BUFSZ;

	Tokenizer (const char* src, int bufsize) {
		buf = new Token[bufsize * sizeof(Token)];
		BUFSZ = bufsize + KEEP_TOKENS;

		cur_src = src;
		cur_tok = &buf[0];
		
		lines.lines.clear();
		lines.lines.reserve(1024*8);

		cur_src = tokenize(cur_tok, buf+BUFSZ, cur_src, lines);

		tokenize_count = 1;
	}
	~Tokenizer () {
		delete[] buf;
	}

	// our lookahead rage moves over the end of buf
	// copy visible window (LOOKBACK - LOOKAHEAD) from end of buffer to start of buffer
	// and fill rest of buffer with new tokens
	void refill_buf () {
		ZoneScoped;
		
		Token* src = cur_tok - LOOKBACK;
		int count = LOOKBACK + LOOKAHEAD - 1; // -1 since that one token was outside the window (that's what triggered refill_buf)

		assert(              buf+count <= buf+BUFSZ);
		assert(src >= buf && src+count <= buf+BUFSZ);

		memmove(buf, src, KEEP_TOKENS * sizeof(Token));

		cur_tok = buf + LOOKBACK;
		Token* new_toks = buf + KEEP_TOKENS;

		_DBG_CLEAR(new_toks, _DBG_MAGIC_UNINIT, (buf+BUFSZ - new_toks) * sizeof(Token));

		cur_src = tokenize(new_toks, buf+BUFSZ, cur_src, lines);

		tokenize_count++;
	}

	void eat () {
		cur_tok++;

		if (cur_tok > buf+BUFSZ - LOOKAHEAD)
			refill_buf();
	}

	// peek at a token in the currently visible window of i in [-LOOKABACK, LOOKAHEAD)
	// i = -1 is the last eaten token (only if token was actually eaten, be careful)
	// i = 0 is the next to be eaten token, 1 is the one after that etc.
	Token& operator[] (int i) {
		assert(i >= -LOOKBACK && i < LOOKAHEAD);
		assert(cur_tok + i >= buf && cur_tok + i < buf+BUFSZ);

		return cur_tok[i];
	}
};