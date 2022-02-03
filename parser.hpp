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

#if TRACY_ENABLE
inline size_t ast_nodes;
#endif

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

#include "types.hpp"

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

// TODO: fix this
inline void dbg_print (AST* node, int depth=0) {
	if (!node) return;
	
	auto indent = [] (int depth) {
		for (int i=0; i<depth; ++i)
			printf("  ");
	};

	indent(depth);
	printf("%s", ASTKind_str[node->kind]);

	switch (node->kind) {
		case A_LITERAL: { auto* lit = (AST_literal*)node;
			std::string str(lit->src_tok->source.text());
			printf(" %s\n", str.c_str());
		} break;

		case A_VARDECL: { auto* var = (AST_vardecl*)node;
			std::string str(var->ident);
			printf(" %s\n", str.c_str()); // , Type_str[var->valtype]
		} break;

		case A_VAR: { auto* var = (AST_var*)node;
			std::string str(var->ident);
			printf(" %s\n", str.c_str());
		} break;

		case A_UNOP: { auto* op = (AST_unop*)node;
			printf("(%s)", OpType_str[op->op]);

			printf(" (\n");
			dbg_print(op->operand, depth+1);
			indent(depth); printf(")\n");
		} break;

		case A_BINOP: 
		case A_ASSIGNOP: { auto* op = (AST_binop*)node;
			printf("(%s) (\n", OpType_str[op->op]);

			dbg_print(op->lhs, depth+1);
			dbg_print(op->rhs, depth+1);

			indent(depth); printf(")\n");
		} break;
			
		case A_BLOCK: { auto* block = (AST_block*)node;
			printf(" (\n");
			for (auto* n : block->statements)
				dbg_print(n, depth+1);
			indent(depth); printf(")\n");
		} break;
			
		case A_IF:
		case A_SELECT: { auto* aif = (AST_if*)node;
			printf(" (\n");

			dbg_print(aif->cond     , depth+1);
			dbg_print(aif->if_body , depth+1);
			if (aif->else_body)
				dbg_print(aif->else_body, depth+1);

			indent(depth); printf(")\n");
		} break;

		case A_WHILE:
		case A_DO_WHILE:
		case A_FOR: { auto* loop = (AST_loop*)node;
			printf(" (\n");
			if (loop->start) {
				dbg_print(loop->start, depth+1);
			}

			dbg_print(loop->cond, depth+1);

			if (loop->end) {
				dbg_print(loop->end, depth+1);
			}
			
			dbg_print(loop->body, depth+1);

			indent(depth); printf(")\n");
		} break;

		case A_STRUCTDEF: { auto* struc = (AST_structdef*)node;
			
			printf(" (\n");
			for (auto* n : struc->members)
				dbg_print(n, depth+1);
			indent(depth); printf(")\n");
			
		} break;

		case A_FUNCDEF: { auto* f = (AST_funcdef*)node;
			std::string str(f->ident);
			printf(" %s\n", str.c_str());

			depth++;

			
			indent(depth); printf("args: (\n");
			for (auto* arg : f->args)
				dbg_print(arg, depth+1);
			indent(depth); printf(")\n");
			
			indent(depth); printf("rets: (\n");
			for (auto* ret : f->rets)
				dbg_print(ret, depth+1);
			indent(depth); printf(")\n");

			indent(depth); printf("(\n");
			dbg_print(f->body, depth+1);
			indent(depth); printf(")\n");
		} break;

		case A_CALLARG: { auto* arg = (AST_callarg*)node;
			if (!arg->ident.empty()) {
				std::string str(arg->ident);
				printf(" %s=", str.c_str());
			}
			
			printf(" (\n");
			dbg_print(arg->expr, depth+1);
			indent(depth); printf(")\n");
		} break;
		case A_CALL: { auto* call = (AST_call*)node;
			std::string str(call->ident);
			printf(" %s (\n", str.c_str());
			for (auto* arg : call->args)
				dbg_print(arg, depth+1);
			indent(depth); printf(")\n");
		} break;
		
		case A_RETURN: { auto* ret = (AST_return*)node;
			printf(" (\n");
			for (auto* arg : ret->args)
				dbg_print(arg, depth+1);
			indent(depth); printf(")\n");
		} break;
		case A_BREAK:
		case A_CONTINUE: {
			printf("\n");
		} break;

		INVALID_DEFAULT;
	}
}

struct Parser {
	Token* tok;

	void throw_error_after (const char* errstr, Token const& after_tok) {
		throw CompilerExcept{errstr, {after_tok.source.end, after_tok.source.end+1}};
	}
	void throw_error (const char* errstr, Token const& tok) {
		throw CompilerExcept{errstr, tok.source };
	}

	void eat_semicolon () {
		if (tok->type != T_SEMICOLON)
			throw_error_after("syntax error: ';' expected", tok[-1]);
		tok++;
	}

	template <typename T, typename FUNC>
	arrview<T> comma_seperated_list (FUNC element, TokenType endtok) {
		smallvec<T, 32> tmp;

		while (tok->type != endtok) {
			tmp.push( element() );

			if (tok->type == T_COMMA) {
				tok++;
			}
			else if (tok->type != endtok) {
				throw_error_after("syntax error: ',' or ')' expected!", tok[-1]); // TODO: use endtok
			}
		}

		T* arr = g_allocator.alloc_array<T>(tmp.count);
		memcpy(arr, tmp.data, tmp.count * sizeof(T));
		return { arr, tmp.count };
	}

	//    (<expression>)                               -> Parenthesized expression 
	// or function call  -> function call with argument expressions
	// or <literal>
	AST* atom () {
		switch (tok->type) {

			case T_PAREN_OPEN: {
				// expression in parentheses
				tok++;

				AST* result = expression(0);

				if (tok->type != T_PAREN_CLOSE)
					throw_error_after("syntax error: parenthesis '(' not closed", tok[-1]);
				tok++;

				return result;
			}

			case T_IDENTIFIER: {
				// func call
				if (tok[1].type == T_PAREN_OPEN) {
					return call();
				}
				// variable
				else {
					auto* var = ast_alloc<AST_var>(A_VAR, tok);
					var->ident = tok->source.text();

					tok++;
					return var;
				}
			}

			case T_LITERAL: {
				auto* lit = ast_alloc<AST_literal>(A_LITERAL, tok);
				lit->type  = Typeref::RValue( BASIC_TYPES[tok->lit_type] );
				lit->value = tok->lit_val;
				tok++;
				return lit;
			}
			
			default: {
				throw_error("syntax error: number or variable expected", *tok);
				return nullptr;
			}
		}
	}

	// expression consisting of atoms combined with unary, binary or ternary operators parsed according to associativity and precedence rules
	AST* expression (unsigned min_prec) {

		AST* lhs;

		// unary prefix operators are right associative, so are parsed by recursing into expression(prec) with prec = unary op precendence
		if (is_unary_prefix_op(tok->type)) {
			unsigned prec = un_prec(tok->type);
			auto* unary_op = ast_alloc<AST_unop>(A_UNOP, tok);
			unary_op->op = tok2unop(tok->type);
			tok++;

			unary_op->operand = expression(prec);
			lhs = unary_op;
		}
		// once all unary operators are parsed right-recursion stops
		else {
			lhs = atom();
		}

		// unary postfix operators are left-associative, so are parsed by a loop
		while (is_unary_postfix_op(tok->type)) {
			unsigned prec = un_prec(tok->type);
			if (prec < min_prec) // handle precedence rules
				return lhs;

			auto* post_op = ast_alloc<AST_unop>(A_UNOP, tok);
			post_op->op = tok2unop(tok->type);
			tok++;

			post_op->operand = lhs;
			lhs = post_op;
		}

		// binary and ternary operators
		while (is_binary_or_ternary_op(tok->type)) {
			unsigned prec  = bt_prec( tok->type);
			unsigned assoc = bt_assoc(tok->type);

			if (prec < min_prec)
				break; // handle precedence rules

			Token* op_tok = tok++;

			AST* rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			// normal binary operator
			if (op_tok->type != T_QUESTIONMARK) {
				auto* op = ast_alloc<AST_binop>(A_BINOP, op_tok);
				op->op = tok2binop(op_tok->type);
				op->lhs = lhs;
				op->rhs = rhs;
				lhs = op;
			}
			// special case: ternary operator
			else {
				if (tok->type != T_COLON)
					throw_error_after("syntax error: ':' expected after true case of select operator", tok[-1]);
				tok++;

				auto* op = ast_alloc<AST_if>(A_SELECT, op_tok);
				op->cond     = lhs;
				op->if_body  = rhs;

				assert(assoc == RIGHT_ASSOC);
				op->else_body = expression(prec);

				lhs = op;
			}
		}

		return lhs;
	}

	//    <varname> :              -> variable declaration with inferred type (must be followed by  = <expression>  to infer the type from)
	// or <varname> : <typename>   -> variable declaration with explicit type
	AST_vardecl* var_decl (int allow_init=true) {
		if (!(tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON))
			throw_error("syntax error: expected variable declaration", *tok);

		auto* var = ast_alloc<AST_vardecl>(A_VARDECL, tok);
		var->ident = tok[0].source.text();

		tok += 2;

		// type specifier
		if (tok->type == T_IDENTIFIER) {
			// ident specifies type, but these identifiers can only be matched to AST_Type's in the later phase
			// leave later phase to find this on it's own
			
			//auto ident = tok->source.text();

			tok++;
		}
		else {
			// no type identifier, type will have to be inferred from initialization

			if (tok->type != T_ASSIGN)
				throw_error_after("syntax error: \neither specify type during variable declaration with \"<var> : <type>;\"\n"
				                                  "or let type be inferred with \"<var> := <expr>;\"", tok[-1]);
		}

		if (tok->type == T_ASSIGN) {
			if (!allow_init)
				throw_error("syntax error: vardecl initialization not allowed in struct (yet)", *tok);

			tok++;
			var->init = expression(0);
		}
		return var;
	}

	//    <expression>
	// or <expression> = <expression>
	AST* assignment_or_expression () {
		AST* expr = expression(0);

		// lhs = rhs   or  lhs += rhs  etc.
		if (is_binary_assignemnt_op(tok->type)) {
			auto* op = ast_alloc<AST_binop>(A_ASSIGNOP, tok);
			op->op = tok2assignop(tok->type);
			tok++;

			op->lhs = expr;
			op->rhs = expression(0);
			return op;
		}
		return expr;
	}

	//    <expression>
	// or <expression> = <expression>
	// or <vardecl> = <expression>   ie.  <varname> : [typename] = <expression>
	AST* decl_or_assignment_or_expression () {
		// lhs is var decl
		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON) {
			return var_decl();
		}
		// lhs is expression
		else {
			return assignment_or_expression();
		}
	}

	//    <expression>
	// or <argname> = <expression>
	AST_callarg* call_arg (TokenType endtok) {
		auto* arg = ast_alloc<AST_callarg>(A_CALLARG, tok);
		arg->ident = strview();

		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_ASSIGN) {
			arg->ident = tok[0].source.text();
			tok += 2;
		}

		arg->expr = expression(0);
		return arg;
	}

	// (<call_arg>, <call_arg> etc.)
	arrview<AST_callarg*> call_args () {
		assert(tok->type == T_PAREN_OPEN);
		tok++;

		auto arr = comma_seperated_list<AST_callarg*>([this] () {
			return call_arg(T_PAREN_CLOSE);
		}, T_PAREN_CLOSE);

		tok++; // T_PAREN_CLOSE
		return arr;
	}
	// <call_arg>, <call_arg> etc.
	arrview<AST_callarg*> return_args () {

		auto arr = comma_seperated_list<AST_callarg*>([this] () {
			return call_arg(T_SEMICOLON);
		}, T_SEMICOLON);

		return arr;
	}

	// <funcname><call_args>
	AST* call () {
		auto* call = ast_alloc<AST_call>(A_CALL, tok);
		call->ident = tok->source.text();
		tok++;

		call->args = call_args();

		return call;
	}

	// return <return_args>;
	AST* return_statement () {
		auto* ast = ast_alloc<AST_return>(A_RETURN, tok++);

		ast->args = return_args();

		eat_semicolon();
		return ast;
	}

	// parses  if <cond> {} elif <cond> {} elif <cond> else {} into a recursive if-else chain (else body points to new recursive if-else for elif)
	// where each elif and else is optional (else = null)
	AST* if_statement () {
		auto* aif = ast_alloc<AST_if>(A_IF, tok++); 

		aif->cond       = expression(0);

		aif->if_body   = block();
		aif->else_body = elif_statement();

		return aif;
	}
	AST* elif_statement () {
		if (tok->type == T_ELIF) {
			// code is same as if except that keyword happens to be elif instead of if (this is why C++ does else if)
			// solve via recurse
			return if_statement();
		}
		if (tok->type == T_ELSE) {
			tok++;
			return block();
		}
		return nullptr;
	}

	// while <cond> <block>
	AST* while_loop () {
		auto* loop = ast_alloc<AST_loop>(A_WHILE, tok++);

		loop->cond = expression(0);
		loop->body = block();

		return loop;
	}

	// do <block> while <cond>
	// NOTE: that <cond> has special scoping rules to allow it to access objects from inside the block
	AST* do_while_loop () {
		auto* loop = ast_alloc<AST_loop>(A_DO_WHILE, tok++);

		loop->body = block();

		if (tok->type != T_WHILE)
			throw_error_after("syntax error: while expected after do block!", tok[-1]);
		tok++;

		loop->cond = expression(0);
		eat_semicolon();

		return loop;
	}

	// for [start]; <cond>; [end] <block>
	AST* for_loop () {
		auto* loop = ast_alloc<AST_loop>(A_FOR, tok++);

		loop->start = decl_or_assignment_or_expression();
		eat_semicolon();

		loop->cond  = expression(0);
		eat_semicolon();

		loop->end   = decl_or_assignment_or_expression();

		loop->body = block();

		return loop;
	}
	
	AST* struct_def () {
		auto* struc = ast_alloc<AST_structdef>(A_STRUCTDEF, tok);
		tok++;

		if (tok->type != T_IDENTIFIER)
			throw_error_after("syntax error: function identifer expected!", tok[-1]);
		struc->ident = tok->source.text();
		tok++;

		smallvec<AST_vardecl*, 16> members;

		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error: '{' expected", *tok);
		tok++;

		while (tok->type != T_BLOCK_CLOSE) {
			auto* s = var_decl(false);
			if (s)
				members.push(s);
			eat_semicolon();
		}

		auto* arr = g_allocator.alloc_array<AST_vardecl*>(members.count);
		memcpy(arr, members.data, members.count * sizeof(members[0]));
		struc->members = { arr, members.count };

		if (tok->type != T_BLOCK_CLOSE)
			throw_error("syntax error, '}' expected", *tok);
		tok++;

		return struc;
	}
	
	AST_vardecl* funcdecl_arg () {
		auto* decl = (AST_vardecl*)var_decl();

		if (decl->init) {
			// TOOD: implement at const-foldable expression for default args at the very least
			// better yet allow things like  sqrt(5)  or even custom compile-time const functions to be called as well
			// or just const values in general (const globals or const locally captured vars)
			// the question is where the const folding happens -> wait until I get to actually implementing compile-time execution
			if (decl->init->kind != A_LITERAL)
				throw_error("syntax error: only literals allowed as default argument values (for now)", *decl->init->src_tok);
			
			assert(decl->init->type.ty && decl->init->type.rval);
		}

		return decl;
	}

	arrview<AST_vardecl*> funcdecl_arglist () {
		assert(tok->type == T_PAREN_OPEN);
		tok++;

		auto arr = comma_seperated_list<AST_vardecl*>([this] () {
			return funcdecl_arg();
		}, T_PAREN_CLOSE);

		tok++; // T_PAREN_CLOSE
		return arr;
	}

	//    func <funcname> (<arg_decl>, <arg_decl>, ...) <block>
	// or func <funcname> (<arg_decl>, <arg_decl>, ...) = (<ret_decl>, <ret_decl>) <block>
	AST* function_def () {
		auto* func = ast_alloc<AST_funcdef>(A_FUNCDEF, tok);
		tok++;

		if (tok->type != T_IDENTIFIER)
			throw_error_after("syntax error: function identifer expected!", tok[-1]);
		func->ident = tok->source.text();
		tok++;

		if (tok->type != T_PAREN_OPEN)
			throw_error_after("syntax error: '(' expected after function identifer!", tok[-1]);

		func->args = funcdecl_arglist();

		// implicit (void) return list
		if (tok->type != T_ASSIGN) {
			// rets already empty
		}
		// explicit return list
		else {
			tok++;

			func->rets = funcdecl_arglist();
		}

		func->body = block();

		return func;
	}

	//    <block>
	// or <if_statement>
	// or <while_loop>
	// or <do_while_loop>
	// or <for_loop>
	// or return <tuple>;   or break;   or continue;
	// or function_def
	// or <assignment_or_expression>;
	// or ;   -> empty statement, which does nothing
	AST* statement () {

		auto eat_semicolon = [this] () {
			if (tok->type != T_SEMICOLON)
				throw_error_after("syntax error: ';' expected", tok[-1]);
			tok++;
		};

		switch (tok[0].type) {
			case T_BLOCK_OPEN: 
				return block();

			case T_IF:
				return if_statement();

			case T_WHILE:
				return while_loop();
			case T_DO:
				return do_while_loop();
			case T_FOR:
				return for_loop();

			case T_RETURN: {
				return return_statement();
			}
			case T_BREAK: {
				auto* ast = ast_alloc<AST>(A_BREAK, tok++);
				eat_semicolon();
				return ast;
			}
			case T_CONTINUE: {
				auto* ast = ast_alloc<AST>(A_CONTINUE, tok++);
				eat_semicolon();
				return ast;
			}
			
			case T_STRUCT: {
				return struct_def();
			}

			case T_FUNC: {
				return function_def();
			}

			// allow empty statements
			case T_SEMICOLON: {
				tok++;
				return nullptr; // no-op
			}

			default: {
				AST* statement = decl_or_assignment_or_expression();
				eat_semicolon();
				return statement;
			}
		}
	}

	AST* _block (Token* blocktok, TokenType endtok) {
		auto* block = ast_alloc<AST_block>(A_BLOCK, blocktok);

		smallvec<AST*, 64> statements;

		while (tok->type != endtok) {
			auto* s = statement();
			if (s)
				statements.push(s);
		}

		auto* arr = g_allocator.alloc_array<AST*>(statements.count);
		memcpy(arr, statements.data, statements.count * sizeof(statements[0]));
		block->statements = { arr, statements.count };

		return block;
	}

	// {
	//   <statement>
	//   <statement>
	//   ...
	// }
	AST* block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error: '{' expected", *tok);

		auto* block = _block(tok++, T_BLOCK_CLOSE);

		if (tok->type != T_BLOCK_CLOSE)
			throw_error("syntax error, '}' expected", *tok);

		tok++;
		return block;
	}

	// <statement>
	// <statement>
	// ...
	AST* file () {
		auto* block = _block(tok, T_EOF);

		if (tok->type != T_EOF)
			throw_error("syntax error: end of file expected", *tok);

		return block;
	}
};
