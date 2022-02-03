#pragma once
#include "basic_types.hpp"

struct AST_type : public AST {
	strview      ident;

	TypeClass    tclass;
	AST*         decl;

	AST_type () {}
	constexpr AST_type (AST ast, strview ident, TypeClass tclass, AST* decl=nullptr):
		AST{ast}, ident{ident}, tclass{tclass}, decl{decl} {}
};

inline constexpr AST_type _TY_BOOL = { cAST(A_TYPE), "bool", TY_BOOL };
inline constexpr AST_type _TY_INT  = { cAST(A_TYPE), "int" , TY_INT  };
inline constexpr AST_type _TY_FLT  = { cAST(A_TYPE), "flt" , TY_FLT  };
inline constexpr AST_type _TY_STR  = { cAST(A_TYPE), "str" , TY_STR  };

inline constexpr AST_type* pTY_BOOL = (AST_type*)&_TY_BOOL;
inline constexpr AST_type* pTY_INT  = (AST_type*)&_TY_INT;
inline constexpr AST_type* pTY_FLT  = (AST_type*)&_TY_FLT;
inline constexpr AST_type* pTY_STR  = (AST_type*)&_TY_STR;

inline constexpr AST_type* BASIC_TYPES[] = {
	pTY_BOOL,
	pTY_INT,
	pTY_FLT,
	pTY_STR,
};
