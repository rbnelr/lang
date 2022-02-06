#pragma once
#include "common.hpp"
#include "tokenizer.hpp"
#include "errors.hpp"

//typedef void (*builtin_func_t)(Value* vals);
typedef void* builtin_func_t;

inline constexpr bool is_binary_or_ternary_op (TokenType tok) {
	return tok >= T_ADD && tok <= T_QUESTIONMARK;
}
inline constexpr bool is_binary_assignemnt_op (TokenType tok) {
	return tok >= T_ASSIGN && tok <= T_MODEQ;
}

inline constexpr bool is_unary_op         (TokenType tok) {
	return (tok >= T_ADD && tok <= T_SUB) || (tok >= T_BIT_NOT && tok <= T_DEC);
}
inline constexpr bool is_unary_prefix_op  (TokenType tok) {
	return (tok >= T_ADD && tok <= T_SUB) || (tok >= T_BIT_NOT && tok <= T_NOT);
}
inline constexpr bool is_unary_postfix_op (TokenType tok) {
	return tok >= T_INC && tok <= T_DEC;
}

/*
//  0  T_QUESTIONMARK
//  1  T_OR
//  2  T_AND
//  3  T_EQUALS, T_NOT_EQUALS
//  4  T_LESS, T_LESSEQ, T_GREATER, T_GREATEREQ
//  5  T_BIT_OR
//  6  T_BIT_XOR
//  7  T_BIT_AND
//  8  T_ADD, T_SUB
//  9  unary T_ADD, T_SUB
// 10  T_MUL, T_DIV, T_MOD
// 11  T_NOT, T_BITNOT
// 12  T_INC, T_DEC
// 13  T_MEMBER
*/
inline constexpr uint8_t BINARY_OP_PRECEDENCE[] = {
	  8, // T_ADD
	  8, // T_SUB
	 10, // T_MUL
	 10, // T_DIV
	 10, // T_MOD
	 
	  7, // T_BIT_AND
	  5, // T_BIT_OR
	  6, // T_BIT_XOR
	 
	  2, // T_AND
	  1, // T_OR
	 
	  4, // T_LESS
	  4, // T_LESSEQ
	  4, // T_GREATER
	  4, // T_GREATEREQ
	  3, // T_EQUALS
	  3, // T_NOT_EQUALS

	 13, // T_MEMBER
	 
	  0, // T_QUESTIONMARK
	
	255, // T_BIT_NOT
	255, // T_NOT
	255, // T_INC
	255, // T_DEC
};
inline constexpr uint8_t UNARY_OP_PRECEDENCE[] = {
	  9, // T_ADD
	  9, // T_SUB
	255, // T_MUL
	255, // T_DIV
	255, // T_MOD

	255, // T_BIT_AND
	255, // T_BIT_OR
	255, // T_BIT_XOR
	
	255, // T_AND
	255, // T_OR

	255, // T_LESS
	255, // T_LESSEQ
	255, // T_GREATER
	255, // T_GREATEREQ
	255, // T_EQUALS
	255, // T_NOT_EQUALS

	255, // T_MEMBER

	255, // T_QUESTIONMARK
	
	 11, // T_BIT_NOT
	 11, // T_NOT
	 12, // T_INC
	 12, // T_DEC
};

enum Associativity : uint8_t {
	LEFT_ASSOC=0,
	RIGHT_ASSOC=1,
};
inline constexpr Associativity BINARY_OP_ASSOCIATIVITY[] = { // 0 = left (left to right execution)  1 = right
	LEFT_ASSOC, // T_ADD
	LEFT_ASSOC, // T_SUB
	LEFT_ASSOC, // T_MUL
	LEFT_ASSOC, // T_DIV
	LEFT_ASSOC, // T_REMAINDER

	LEFT_ASSOC, // T_BIT_AND
	LEFT_ASSOC, // T_BIT_OR
	LEFT_ASSOC, // T_BIT_XOR

	LEFT_ASSOC, // T_AND
	LEFT_ASSOC, // T_OR

	LEFT_ASSOC, // T_LESS
	LEFT_ASSOC, // T_LESSEQ
	LEFT_ASSOC, // T_GREATER
	LEFT_ASSOC, // T_GREATEREQ
	LEFT_ASSOC, // T_EQUALS
	LEFT_ASSOC, // T_NOT_EQUALS

	LEFT_ASSOC, // T_MEMBER

	RIGHT_ASSOC, // T_QUESTIONMARK
};

inline unsigned bt_prec (TokenType tok) {
	assert(is_binary_or_ternary_op(tok));
	return BINARY_OP_PRECEDENCE[tok - T_ADD];
}
inline unsigned un_prec (TokenType tok) {
	assert(is_unary_op(tok));
	return UNARY_OP_PRECEDENCE[tok - T_ADD];
}
inline unsigned bt_assoc (TokenType tok) {
	assert(is_binary_or_ternary_op(tok));
	return (bool)BINARY_OP_ASSOCIATIVITY[tok - T_ADD];
}

// called "kind" instead of "type" to avoid confusion with types in the actual language, which we also need to store
enum ASTKind : uint8_t {
	A_TYPE,

	A_BLOCK,
	A_TUPLE,

	// values
	A_LITERAL,

	A_VARDECL,
	A_VAR,

	A_VARARGS,

	A_STRUCTDEF,

	A_FUNCDEF,

	A_CALLARG,
	A_CALL,

	// flow control
	A_IF,

	A_WHILE,
	A_DO_WHILE,
	A_FOR,

	A_RETURN,
	A_BREAK,
	A_CONTINUE,

	A_UNOP,
	A_BINOP,
	A_ASSIGNOP,
	// ternary operator
	A_SELECT,
};
inline const char* ASTKind_str[] = {
	"A_TYPE",

	"A_BLOCK",
	"A_TUPLE",

	"A_LITERAL",

	"A_VARDECL",
	"A_VAR",

	"A_VARARGS",

	"A_STRUCTDEF",

	"A_FUNCDEF",

	"A_CALLARG",
	"A_CALL",

	"A_IF",

	"A_WHILE",
	"A_DO_WHILE",
	"A_FOR",

	"A_RETURN",
	"A_BREAK",
	"A_CONTINUE",

	"A_UNOP",
	"A_BINOP",
	"A_ASSIGNOP",
	"A_SELECT",
};

enum OpType {
	OP_ASSIGN=0, // used for =

	// binary operators
	OP_ADD, // used for + and +=
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_MOD,

	OP_BIT_AND,
	OP_BIT_OR,
	OP_BIT_XOR,

	OP_LOGICAL_AND,
	OP_LOGICAL_OR,

	OP_LESS,
	OP_LESSEQ,
	OP_GREATER,
	OP_GREATEREQ,
	OP_EQUALS,
	OP_NOT_EQUALS,

	OP_MEMBER,

	// unary operators
	OP_POSITIVE, // usually a no-op, but possibly could do something with operator overloading
	OP_NEGATE,
	OP_BIT_NOT,
	OP_LOGICAL_NOT,
	OP_INC,
	OP_DEC,
};
inline const char* OpType_str[] = {
	"=",

	"+",
	"-",
	"*",
	"/",
	"%",

	"&",
	"|",
	"^",

	"&&",
	"||",

	"<",
	"<=",
	">",
	">=",
	"==",
	"!=",

	".",

	"+",
	"-",
	"~",
	"!",
	"x++",
	"x--",
};


inline constexpr OpType tok2binop (TokenType tok) {
	return (OpType)( tok + (OP_ADD - T_ADD) );
}
inline constexpr OpType tok2unop (TokenType tok) {
	switch (tok) {
		case T_ADD: return OP_POSITIVE;
		case T_SUB: return OP_NEGATE;
		default:    return (OpType)( tok + (OP_BIT_NOT - T_BIT_NOT) );
	}
}
inline constexpr OpType tok2assignop (TokenType tok) {
	return (OpType)( tok + (OP_ASSIGN - T_ASSIGN) );
}


namespace llvm { // forward decls to associate llvm IR with AST nodes
	class Type;
	class Value;
	class Function;
	class StructType;
}

struct AST_type;

struct Typeref {
	AST_type* ty   = nullptr;
	bool      rval = false;
	
	static Typeref LValue (AST_type* ty) { return { ty, false }; }
	static Typeref RValue (AST_type* ty) { return { ty, true }; }
};

struct AST {
	ASTKind      kind;

	Token*       src_tok;

	Typeref      type;
};

inline constexpr AST cAST (ASTKind type, AST_type* valtype=nullptr) {
	return { type, nullptr, valtype };
}

#if TRACY_ENABLE
inline size_t ast_nodes;
#endif

template <typename T>
inline T* ast_alloc (ASTKind kind, Token* tok) {
	T* ret = g_allocator.alloc<T>();
	
	ret->kind     = kind;
	ret->src_tok  = tok;

#if TRACY_ENABLE
	ast_nodes++;
#endif
	return ret;
}

struct AST_block : public AST {
	arrview<AST*> statements;
};

struct AST_literal : public AST {
	Value        value;
};

struct AST_vardecl : public AST {
	strview      ident;

	AST*         init         = nullptr;   // initialization during declaration

	bool         is_arg       = false; // for IR gen, is this variable a function argument?
	
	llvm::Type*  llvm_type    = nullptr;
	llvm::Value* llvm_value   = nullptr;
	unsigned     llvm_GEP_idx = 0; // only for struct members

	Token* get_type_tok () {
		if (src_tok[2].type != T_IDENTIFIER)
			return nullptr;

		return &src_tok[2];
	}
};

// TODO: either a variable identifier or a struct member identifer
//       could split these into seperate AST types if desired
struct AST_var : public AST {
	strview      ident;
	AST_vardecl* decl = nullptr;
};

struct AST_structdef : public AST {
	strview      ident;
	
	arrview<AST_vardecl*> members;
	
	llvm::StructType*  llvm_struct = nullptr;
};

struct AST_funcdef : public AST {
	strview      ident;
	
	arrview<AST_vardecl*> args;
	arrview<AST_vardecl*> rets;

	//AST_structdef* ret_struct;

	AST*         body         = nullptr;
	
	void*        builtin_func_ptr = nullptr;

	llvm::Function* llvm_func = nullptr;
};

struct AST_callarg : public AST {
	strview      ident; // empty if positional argument
	AST*         expr       = nullptr;

	AST_vardecl* decl       = nullptr;
};
struct AST_call : public AST {
	strview      ident;

	// source code args
	arrview<AST_callarg*> args;

	AST*         fdef       = nullptr; // either points to AST_funcdef or AST_funcdef_builtin, check via AST.type

	// resolved args, ie. list of expressions each corresponding to one arg in fdef->args
	// where 0-n expressions can appear in place of the last varargs arg
	// for non-provided default args  AST_vardecl->init is the expression
	arrview<AST*> resolved_args;
};

struct AST_if : public AST {
	AST*         cond      = nullptr;
	AST*         if_body   = nullptr;
	AST*         else_body = nullptr;
};
struct AST_loop : public AST {
	AST*         start     = nullptr;
	AST*         cond      = nullptr;
	AST*         end       = nullptr;
	AST*         body      = nullptr;
};

struct AST_unop : public AST {
	OpType       op;
	AST*         operand   = nullptr;
};
struct AST_binop : public AST {
	OpType       op;
	AST*         lhs       = nullptr;
	AST*         rhs       = nullptr;
};

struct AST_return : public AST {
	arrview<AST_callarg*> args;
};

struct AST_Module {
	std::string filename;

	AST* ast;

	std::vector<AST_funcdef*>   funcs;
	std::vector<AST_structdef*> structs;
};

void dbg_print (AST* node, int depth=0);

AST* parse (Token* tokens);
