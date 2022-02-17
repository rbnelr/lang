#pragma once
#include "common.hpp"
#include "basic_types.hpp"

enum TokenType : uint8_t {
	T_EOF   =0,

	T_LITERAL_BOOL,
	T_LITERAL_INT,
	T_LITERAL_FLT,
	T_LITERAL_STR,
	
	T_IDENTIFIER,
	T_FUNC,
	T_STRUCT,

	T_IF,
	T_ELIF,
	T_ELSE,

	T_WHILE,      
	T_FOR,        
	T_DO,         

	T_RETURN,     
	T_BREAK,      
	T_CONTINUE,   
	T_GOTO,       

	//
	T_COLON         ,//=':',
	T_SEMICOLON     ,//=';',
	T_COMMA         ,//=',',
	
	T_PAREN_OPEN    ,//='(',
	T_PAREN_CLOSE   ,//=')',
	T_BLOCK_OPEN    ,//='{',
	T_BLOCK_CLOSE   ,//='}',
	T_INDEX_OPEN    ,//='[',
	T_INDEX_CLOSE   ,//=']',
	
	T_ADD           ,//='+',
	T_SUB           ,//='-',
	T_MUL           ,//='*',
	T_DIV           ,//='/',
	T_MOD           ,//='%',
	
	T_BIT_AND       ,//='&',
	T_BIT_OR        ,//='|',
	T_BIT_XOR       ,//='^',
	
	T_AND           ,//='&' + 32,
	T_OR            ,//='|' + 32,
	
	T_LESS          ,//='<',
	T_LESSEQ        ,//='<' + 32,
	T_GREATER       ,//='>',
	T_GREATEREQ     ,//='>' + 32,
	T_EQUALS        ,//='=' + 32,
	T_NOT_EQUALS    ,//='!' + 32,
	
	T_MEMBER        ,//='.',
	
	T_QUESTIONMARK  ,//='?',
	
	T_BIT_NOT       ,//='~',
	T_NOT           ,//='!',
	T_INC           ,//='+' + 64,
	T_DEC           ,//='-' + 64,
	
	T_ASSIGN        ,//='=' + 32,
	T_ADDEQ         ,//='+' + 32,
	T_SUBEQ         ,//='-' + 32,
	T_MULEQ         ,//='*' + 32,
	T_DIVEQ         ,//='/' + 32,
	T_MODEQ         ,//='%' + 32,
};

inline constexpr const char* TokenType_str[] = {
	"T_EOF",         
	
	"T_LITERAL_BOOL",
	"T_LITERAL_INT", 
	"T_LITERAL_FLT", 
	"T_LITERAL_STR", 
	
	"T_IDENTIFIER",  

	"T_FUNC",        
	"T_STRUCT",      
	
	"T_IF",          
	"T_ELIF",        
	"T_ELSE",        

	"T_WHILE",       
	"T_FOR",         
	"T_DO",          

	"T_RETURN",      
	"T_BREAK",       
	"T_CONTINUE",    
	"T_GOTO",        

	"T_COLON",       
	"T_SEMICOLON",   
	"T_COMMA",       

	"T_PAREN_OPEN",  
	"T_PAREN_CLOSE", 
	"T_BLOCK_OPEN",  
	"T_BLOCK_CLOSE", 
	"T_INDEX_OPEN",  
	"T_INDEX_CLOSE", 

	"T_ADD",         
	"T_SUB",         
	"T_MUL",         
	"T_DIV",         
	"T_MOD",         

	"T_BIT_AND",     
	"T_BIT_OR",      
	"T_BIT_XOR",     

	"T_AND",         
	"T_OR",          

	"T_LESS",        
	"T_LESSEQ",      
	"T_GREATER",     
	"T_GREATEREQ",   
	"T_EQUALS",      
	"T_NOT_EQUALS",  

	"T_MEMBER",      

	"T_QUESTIONMARK",

	"T_BIT_NOT",     
	"T_NOT",         
	"T_INC",         
	"T_DEC",         

	"T_ASSIGN",      
	"T_ADDEQ",       
	"T_SUBEQ",       
	"T_MULEQ",       
	"T_DIVEQ",       
	"T_MODEQ",       
};
inline constexpr const char* TokenType_char[] = {
	"<EOF>"       ,
              
	"literal_bool",
	"literal_int" ,
	"literal_flt" ,
	"literal_str" ,
              
	"identifier"  ,
              
	"func"        ,
	"struct"      ,
              
	"if"          ,
	"elif"        ,
	"else"        ,
              
	"while"       ,
	"for"         ,
	"do"          ,
              
	"return"      ,
	"break"       ,
	"continue"    ,
	"goto"        ,
              
	":"           ,
	";"           ,
	","           ,
              
	"("           ,
	")"           ,
	"{"           ,
	"}"           ,
	"["           ,
	"]"           ,
              
	"+"           ,
	"-"           ,
	"*"           ,
	"/"           ,
	"%"           ,
              
	"&"           ,
	"|"           ,
	"^"           ,
              
	"&&"          ,
	"||"          ,
              
	"<"           ,
	"<="          ,
	">"           ,
	">="          ,
	"=="          ,
	"!="          ,
              
	"."           ,
              
	"?"           ,
              
	"~"           ,
	"!"           ,
	"++"          , // x++ is more readable, but profiling.hpp  wants the actual token chars
	"--"          ,
              
	"="           ,
	"+="          ,
	"-="          ,
	"*="          ,
	"/="          ,
	"%="          ,
};


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

	// char index of start character (0-based)
	uint16_t    start_charno;

	// lengh of string starting from start (saturated on overflow)
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
	const char* cur_char;
	const char* cur_line;
	size_t      cur_lineno;

	Token*      cur_tok;
	
	static inline constexpr int LOOKBACK = 1;  // how many future tokens are safe to read
	static inline constexpr int LOOKAHEAD = 2; // how many past   tokens are safe to read
	
	static inline constexpr int WINDOW_SIZE = LOOKBACK + LOOKAHEAD; // how many buffered tokens are valid to read at any one point
	static inline constexpr int KEEP_TOKENS = WINDOW_SIZE -1; // how many tokens are kept in refill_buf() when reaching the end of the buffer

	static inline constexpr int BUFSZ = 256; // if  sizeof(Token) * BUFSZ  fits into a cpu cache level that may improve perf 
	
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