#pragma once
#include "common.hpp"
#include "basic_types.hpp"

#define TOKTYPES \
	/*end of file*/                    \
	X( EOF=0,         "<EOF>" )        \
	/* literals */                     \
	X( LITERAL_BOOL,  "literal_bool" ) \
	X( LITERAL_INT,   "literal_int"  ) \
	X( LITERAL_FLT,   "literal_flt"  ) \
	X( LITERAL_STR,   "literal_str"  ) \
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
enum TokenType : uint8_t {
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



// only for debug printing, so we don't care about handling degenerate cases like 2GB long tokens etc.
// use this to cut down on the size of this struct a little, since every Token and AST node inludes an instance of this

inline size_t saturate16 (size_t x) { return x <= UINT32_MAX ? x : UINT32_MAX; }
inline size_t saturate32 (size_t x) { return x <= UINT16_MAX ? x : UINT16_MAX; }

struct SourceRange {
	// first char as ptr into source (allows me to see things in debugger)
	char const* start;

	// lineno of start character     (1-based to match common text editors)
	uint32_t    start_lineno;

	// char index of start character (0-based TODO: also 1-based?)
	uint16_t    start_charno;

	// lengh of string starting from start (saturated on overflow)
	uint16_t    length;

	// offset of source token relative to start (for binary operators etc.) to show up like ~~~~^~~~~
	uint16_t    arrow;

	strview text () const {
		return strview(start, (size_t)length);
	}

	static SourceRange after_tok (SourceRange& src) {
		SourceRange r;
		r.start  = src.start + src.length;
		r.start_lineno = src.start_lineno;
		r.start_charno = (uint16_t)saturate16(src.start_charno + src.length);
		r.length = 1;
		r.arrow = 0;
		return r;
	}

	static SourceRange range (SourceRange& a, SourceRange& b) {
		SourceRange r = a;
		r.length = (uint16_t)saturate16((size_t)(b.start - a.start) + b.length);
		r.arrow  = 0;
		return r;
	}
	static SourceRange range_with_arrow (SourceRange& a, SourceRange& arrow, SourceRange& b) {
		SourceRange r = a;
		r.length = (uint16_t)saturate16((size_t)(b.start - a.start) + b.length);
		r.arrow  = (uint16_t)saturate16((size_t)(arrow.start - a.start));
		return r;
	}
};




struct Token {
	TokenType    type;
	SourceRange  src;
};

constexpr size_t _sr_sz = sizeof(SourceRange);
constexpr size_t _tok_sz = sizeof(Token);

struct Lexer {
	const char* cur_char;
	const char* cur_line;
	size_t      cur_lineno;

	Token*      cur_tok;
	
	static inline constexpr int LOOKBACK = 1;
	// how many tokes are always valid relative to lookahead with peek(), eg. peek(LOOKAHEAD-1) is safe
	static inline constexpr int LOOKAHEAD = 2;

	static inline constexpr int WINDOW_SIZE = LOOKBACK + LOOKAHEAD; // how many buffered tokens are valid to access at any one point
	static inline constexpr int KEEP_TOKENS = WINDOW_SIZE -1; // how many tokens are kept in refill_buf() when reaching the end of the buffer

	static inline constexpr int BUFSZ = 1024; // if  sizeof(Token) * BUFSZ  fits into a cpu cache level that should improve perf 
	
	Token buf[BUFSZ];

	Lexer (const char* src) {
		cur_char = src;
		
		cur_line = cur_char;
		cur_lineno = 1;

		cur_tok = &buf[0];

		lex(&buf[0], buf+BUFSZ);
	}

	// our lookahead rage moves over the end of buf
	// copy visible window (LOOKBACK - LOOKAHEAD) from end of buffer to start of buffer
	// and fill rest of buffer with new tokens
	_NOINLINE void refill_buf () {
		ZoneScoped;

		Token* src = cur_tok - LOOKBACK;
		int count = LOOKBACK + LOOKAHEAD - 1; // -1 since that one token was outside the window (that's what triggered refill_buf)

		assert(              buf+count <= buf+BUFSZ);
		assert(src >= buf && src+count <= buf+BUFSZ);

		memmove(buf, src, KEEP_TOKENS * sizeof(Token));

		cur_tok = buf + LOOKBACK;
		Token* new_toks = buf + KEEP_TOKENS;

		_DBG_CLEAR(new_toks, _DBG_MAGIC_UNINIT, (buf+BUFSZ - new_toks) * sizeof(Token));

		lex(new_toks, buf+BUFSZ);
	}
	
	void set_source_range_start (SourceRange* r, char const* start) {
		r->start        = start;
		r->start_lineno = (uint32_t)saturate32(cur_lineno                );
		r->start_charno = (uint16_t)saturate16((size_t)(start - cur_line));
	}
	void set_source_range_len (SourceRange* r, ptrdiff_t len) {
		r->length       = (uint16_t)saturate16((size_t)len);
	}

	SourceRange get_source_range (char const* start, char const* end) {
		SourceRange r;
		set_source_range_start(&r, start);
		set_source_range_len(&r, end - start);
		return r;
	}
	
	void parse_lit_bool    (const char* start, const char* end, Value* out_val);
	void parse_lit_integer (const char* start, const char* end, Value* out_val);
	void parse_lit_double  (const char* start, const char* end, Value* out_val);
	void parse_lit_string  (const char* start, const char* end, Value* out_val);

	TypeClass parse_literal (TokenType type, const char* start, const char* end, Value* out_val);

	void lex (Token* first_tok, Token* end_tok);

	_FORCEINLINE void eat () {
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