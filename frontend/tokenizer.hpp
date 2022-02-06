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

std::vector<Token> tokenize (const char* src);
