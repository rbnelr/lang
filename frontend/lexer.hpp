#pragma once
#include "common.hpp"
#include "basic_types.hpp"

#define _TOKENS \
	/* end of file */ \
	X( T_EOF          , "<EOF>"        ) \
	/* alphanumeric+underscore not starting with digit and not matching any keyword */ \
	X( T_IDENTIFIER   , "identifier"   ) \
	/* literals */ \
	X( T_LITERAL_BOOL , "literal_bool" ) /* true ; false */ \
	X( T_LITERAL_INT  , "literal_int"  ) /* 0 ; 5 ; 12345 (unary minus '-' is not part of literal) */ \
	X( T_LITERAL_FLT  , "literal_flt"  ) /* 0.0 ; .3 ; 7. */ \
	X( T_LITERAL_STR  , "literal_str"  ) /* "" ; "hello World" ; "<utf8 string>"  */ \
	/* variable declaration keywords */ \
	X( T_LET          , "let"          ) /* reserved */ \
	X( T_VAR          , "var"          ) \
	X( T_CONST        , "const"        ) /* reserved */ \
	/* function and struct declaration keywords */ \
	X( T_FUNC         , "func"         ) \
	X( T_STRUCT       , "struct"       ) \
    /* control flow */ \
	X( T_IF           , "if"           ) \
	X( T_ELIF         , "elif"         ) \
	X( T_ELSE         , "else"         ) \
    \
	X( T_WHILE        , "while"        ) \
	X( T_FOR          , "for"          ) \
	X( T_DO           , "do"           ) \
    \
	X( T_RETURN       , "return"       ) \
	X( T_BREAK        , "break"        ) \
	X( T_CONTINUE     , "continue"     ) \
	X( T_GOTO         , "goto"         ) \
    /* syntax characters */ \
	X( T_COLON        , ":"            ) \
	X( T_SEMICOLON    , ";"            ) \
	X( T_COMMA        , ","            ) \
	\
	X( T_PAREN_OPEN   , "("            ) \
	X( T_PAREN_CLOSE  , ")"            ) \
	X( T_BLOCK_OPEN   , "{"            ) \
	X( T_BLOCK_CLOSE  , "}"            ) \
	X( T_INDEX_OPEN   , "["            ) \
	X( T_INDEX_CLOSE  , "]"            ) \
	/* single and double char operator */ \
	X( T_ADD          , "+"            ) \
	X( T_SUB          , "-"            ) \
	X( T_MUL          , "*"            ) \
	X( T_DIV          , "/"            ) \
	X( T_MOD          , "%"            ) \
	\
	X( T_BIT_AND      , "&"            ) \
	X( T_BIT_OR       , "|"            ) \
	X( T_BIT_XOR      , "^"            ) \
	\
	X( T_AND          , "&&"           ) \
	X( T_OR           , "||"           ) \
	\
	X( T_LESS         , "<"            ) \
	X( T_LESSEQ       , "<="           ) \
	X( T_GREATER      , ">"            ) \
	X( T_GREATEREQ    , ">="           ) \
	X( T_EQUALS       , "=="           ) \
	X( T_NOT_EQUALS   , "!="           ) \
	\
	X( T_MEMBER       , "."            ) \
	\
	X( T_QUESTIONMARK , "?"            ) \
	\
	X( T_BIT_NOT      , "~"            ) \
	X( T_NOT          , "!"            ) \
	X( T_INC          , "++"           ) \
	X( T_DEC          , "--"           ) \
	\
	X( T_ASSIGN       , "="            ) \
	X( T_ADDEQ        , "+="           ) \
	X( T_SUBEQ        , "-="           ) \
	X( T_MULEQ        , "*="           ) \
	X( T_DIVEQ        , "/="           ) \
	X( T_MODEQ        , "%="           )

#define X(ENUM, CHAR) ENUM,
enum TokenType : uint8_t {
	_TOKENS
};
#undef X

#define X(ENUM, CHAR) STRINGIFY(ENUM),
inline constexpr const char* TokenType_str[] = {
	_TOKENS
};
#undef X

#define X(ENUM, CHAR) CHAR,
inline constexpr const char* TokenType_char[] = {
	_TOKENS
};
#undef X

#undef _TOKENS


// use small ints in SourceRange since it's only for debug info/printing
// so we don't care about handling degenerate cases like 64k long tokens etc.
// use this to cut down on the size of this struct a little, since every Token and AST node inludes an instance of this
// use these saturate functions to avoid wrap around if tokens ever are actually that long (TODO: not tested yet)

//inline size_t saturate16 (size_t x) { return x <= UINT32_MAX ? x : UINT32_MAX; }
//inline size_t saturate32 (size_t x) { return x <= UINT16_MAX ? x : UINT16_MAX; }

struct SourceRange {
	// first char as ptr into source (allows me to see things in debugger)
	char const* start;

	// lineno of start character     (1-based to match common text editors)
	uint32_t    start_lineno;

	// char index of start character in line (column) (0-based)
	uint16_t    start_charno;

	// lengh of range in chars (saturated on overflow)
	uint16_t    length;

	//// offset of source token relative to start (for binary operators etc.) to show up like ~~~~^~~~~
	//uint16_t    arrow;

	strview text () const {
		return strview(start, (size_t)length);
	}

	static SourceRange after_tok (SourceRange& src) {
		SourceRange r;
		r.start  = src.start + src.length;
		r.start_lineno = src.start_lineno;
		//r.start_charno = (uint16_t)saturate16(src.start_charno + src.length);
		r.start_charno = (uint16_t)(src.start_charno + src.length);
		r.length = 1;
		//r.arrow = 0;
		return r;
	}

	static SourceRange range (SourceRange& a, SourceRange& b) {
		SourceRange r = a;
		//r.length = (uint16_t)saturate16((size_t)(b.start - a.start) + b.length);
		r.length = (uint16_t)((size_t)(b.start - a.start) + b.length);
		//r.arrow  = 0;
		return r;
	}
	static SourceRange range_with_arrow (SourceRange& a, SourceRange& arrow, SourceRange& b) {
		SourceRange r = a;
		//r.length = (uint16_t)saturate16((size_t)(b.start - a.start) + b.length);
		r.length = (uint16_t)((size_t)(b.start - a.start) + b.length);
		//r.arrow  = (uint16_t)saturate16((size_t)(arrow.start - a.start));
		return r;
	}
};




struct Token {
	TokenType    type;
	SourceRange  src;
};

constexpr size_t _sr_sz = sizeof(SourceRange);
constexpr size_t _tok_sz = sizeof(Token);

// This lexer uses fills a fixed size buffer of tokens and allows you to consume tokens with eat()
// and view a small windows of past and future tokens with operator[]
struct Lexer {
	const char* source_cur;
	const char* source_end;
	const char* cur_line;
	size_t      cur_lineno;

	Token*      cur_tok;
	
	static inline constexpr int LOOKBACK = 1;  // how many future tokens are safe to read
	static inline constexpr int LOOKAHEAD = 2; // how many past   tokens are safe to read
	
	static inline constexpr int WINDOW_SIZE = LOOKBACK + LOOKAHEAD; // how many buffered tokens are valid to read at any one point
	static inline constexpr int KEEP_TOKENS = WINDOW_SIZE -1; // how many tokens are kept in refill_buf() when reaching the end of the buffer

	static inline constexpr int BUFSZ = 256; // if  sizeof(Token) * BUFSZ  fits into a cpu cache level that may improve perf 
	
	Token buf[BUFSZ];

	// source needs to be properly null terminated, but if a null bytes appears in the middle of the source (before source.size())
	// we will throw an error on the '\0' char
	Lexer (strview source) {
		source_cur = source.data();
		source_end = source.data() + source.size();
		
		cur_line = source_cur;
		cur_lineno = 1;

		cur_tok = &buf[0];

		lex(&buf[0], buf+BUFSZ);
	}

	// our lookahead rage moves over the end of buf
	// copy visible window (LOOKBACK - LOOKAHEAD) from end of buffer to start of buffer
	// and fill rest of buffer with new tokens
	_NOINLINE void refill_buf () {
		Token* src = cur_tok - LOOKBACK;
		
		assert(src >= buf && src+KEEP_TOKENS <= buf+BUFSZ);

		memmove(buf, src, KEEP_TOKENS * sizeof(Token));

		cur_tok = buf + LOOKBACK;
		Token* new_toks = buf + KEEP_TOKENS;

		_DBG_CLEAR(new_toks, _DBG_MAGIC_UNINIT, (buf+BUFSZ - new_toks) * sizeof(Token));

		lex(new_toks, buf+BUFSZ);
	}
	
	void set_source_range_start (SourceRange* r, char const* start) {
		r->start        = start;
		//r->start_lineno = (uint32_t)saturate32(cur_lineno                );
		//r->start_charno = (uint16_t)saturate16((size_t)(start - cur_line));
		r->start_lineno = (uint32_t)(cur_lineno                );
		r->start_charno = (uint16_t)((size_t)(start - cur_line));
		//r->arrow        = 0;
	}
	void set_source_range_len (SourceRange* r, ptrdiff_t len) {
		//r->length       = (uint16_t)saturate16((size_t)len);
		r->length       = (uint16_t)((size_t)len);
	}

	SourceRange get_source_range (char const* start, char const* end) {
		SourceRange r;
		set_source_range_start(&r, start);
		set_source_range_len(&r, end - start);
		return r;
	}
	
	// actually defer the parsing of literals to the parsing (rather than during lexing)
	// since this avoids having to store the Value struct for every single token
	void parse_lit_bool    (const char* start, const char* end, Value* out_val);
	void parse_lit_integer (const char* start, const char* end, Value* out_val);
	void parse_lit_double  (const char* start, const char* end, Value* out_val);
	void parse_lit_string  (const char* start, const char* end, Value* out_val);

	TypeClass parse_literal (TokenType type, const char* start, const char* end, Value* out_val);

	void lex (Token* first_tok, Token* end_tok);

	// consume a token, ie. move the visible window of tokens forward by one
	// occasionally refills the buffer of tokens by lexing more of the source code
	_FORCEINLINE void eat () {
		cur_tok++;

		if (cur_tok > buf+BUFSZ - LOOKAHEAD)
			refill_buf();
	}

	// peek at a token in the currently visible window of i in [-LOOKABACK, LOOKAHEAD)
	// i = -1 is the last eaten token (only if token was actually eaten, be careful)
	// i = 0 is the next to be eaten token, 1 is the one after that etc.
	// NOTE: could be called peek()
	Token& operator[] (int i) {
		assert(i >= -LOOKBACK && i < LOOKAHEAD);
		assert(cur_tok + i >= buf && cur_tok + i < buf+BUFSZ);

		return cur_tok[i];
	}
};