#pragma once
#include "common.hpp"
#include "tokenizer.hpp"
#include "errors.hpp"
#include "basic_types.hpp"

//typedef void (*builtin_func_t)(Value* vals);
typedef void* builtin_func_t;

inline constexpr bool is_binary_or_ternary_op (TokenType tok) {
	return (tok >= T_ADD && tok <= T_NOT_EQUALS) || tok == T_QUESTIONMARK;
}
inline constexpr bool is_binary_assignemnt_op (TokenType tok) {
	return tok >= T_ASSIGN && tok <= T_REMAINDEREQ;
}

inline constexpr bool is_unary_op         (TokenType tok) {
	return (tok >= T_ADD && tok <= T_SUB) || (tok >= T_NOT && tok <= T_DEC);
}
inline constexpr bool is_unary_prefix_op  (TokenType tok) {
	return (tok >= T_ADD && tok <= T_SUB) || tok == T_NOT;
}
inline constexpr bool is_unary_postfix_op (TokenType tok) {
	return tok >= T_INC && tok <= T_DEC;
}

inline constexpr uint8_t BINARY_OP_PRECEDENCE[] = {
	3, // T_ADD
	3, // T_SUB
	5, // T_MUL
	5, // T_DIV
	5, // T_REMAINDER

	2, // T_LESS
	2, // T_LESSEQ
	2, // T_GREATER
	2, // T_GREATEREQ
	1, // T_EQUALS
	1, // T_NOT_EQUALS

	0, // T_QUESTIONMARK
	
	255, // T_NOT
	255, // T_INC
	255, // T_DEC
};
inline constexpr uint8_t UNARY_OP_PRECEDENCE[] = {
	4,   // T_ADD
	4,   // T_SUB
	255, // T_MUL
	255, // T_DIV
	255, // T_REMAINDER

	255, // T_LESS
	255, // T_LESSEQ
	255, // T_GREATER
	255, // T_GREATEREQ
	255, // T_EQUALS
	255, // T_NOT_EQUALS

	255, // T_QUESTIONMARK
	
	6, // T_NOT
	7, // T_INC
	7, // T_DEC
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

	LEFT_ASSOC, // T_LESS
	LEFT_ASSOC, // T_LESSEQ
	LEFT_ASSOC, // T_GREATER
	LEFT_ASSOC, // T_GREATEREQ
	LEFT_ASSOC, // T_EQUALS
	LEFT_ASSOC, // T_NOT_EQUALS

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

enum ASTType {
	A_BLOCK,
	A_TUPLE,

	// values
	A_LITERAL,

	A_VARDECL,
	A_VAR,

	A_VARARGS,

	A_FUNCDEF,
	A_FUNCDEF_BUILTIN,

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
inline const char* ASTType_str[] = {
	"A_BLOCK",
	"A_TUPLE",

	"A_LITERAL",

	"A_VARDECL",
	"A_VAR",

	"A_VARARGS",

	"A_FUNCDEF",
	"A_FUNCDEF_BUILTIN",

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
	OP_REMAINDER,

	OP_LESS,
	OP_LESSEQ,
	OP_GREATER,
	OP_GREATEREQ,
	OP_EQUALS,
	OP_NOT_EQUALS,

	// unary operators
	OP_POSITIVE, // usually a no-op, but possibly could do something with operator overloading
	OP_NEGATE,
	OP_NOT,
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

	"<",
	"<=",
	">",
	">=",
	"==",
	"!=",

	"+",
	"-",
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
		default:    return (OpType)( tok + (OP_NOT - T_NOT) );
	}
}
inline constexpr OpType tok2assignop (TokenType tok) {
	return (OpType)( tok + (OP_ASSIGN - T_ASSIGN) );
}

struct AST {
	ASTType      type;

	Token const* src_tok;

	AST*         next;
	Type         valtype;
};

template <typename T>
inline T* ast_alloc (ASTType type, Token const* tok) {
	T* ret = g_allocator.alloc<T>();
	
	memset(ret, 0, sizeof(T));

	ret->type     = type;
	ret->src_tok  = tok;
	ret->next     = nullptr;
	return ret;
}

struct AST_block : public AST {
	AST*         statements;
};

struct AST_literal : public AST {
	Value        value;
};

struct AST_vardecl : public AST {
	strview      ident;

	AST*         init;       // initialization during declaration

	size_t       var_id;     // for IR gen
	bool         var_is_arg; // for IR gen, is this variable a function argument?
	
	void*        llvm_value;
};

struct AST_var : public AST {
	strview      ident;
	AST_vardecl* decl;
};

struct AST_funcdecl: public AST  {
	strview      ident;

	size_t       argc;
	AST*         args;

	size_t       retc;
	AST*         rets;
};
struct AST_funcdef : public AST_funcdecl {
	AST*         body;
	size_t       codegen_funcid;
};
struct AST_funcdef_builtin : public AST_funcdecl {
	builtin_func_t func_ptr;
};

struct AST_callarg : public AST {
	strview      ident; // empty if positional argument
	AST*         expr;

	AST_vardecl* decl;
	size_t       decli;
};
struct AST_call : public AST {
	strview      ident;

	size_t       argc;
	AST*         args;

	AST*         fdef; // either points to AST_funcdef or AST_funcdef_builtin, check via AST.type
};

struct AST_if : public AST {
	AST*         cond;
	AST*         if_body;
	AST*         else_body;
};
struct AST_loop : public AST {
	AST*         start;
	AST*         cond;
	AST*         end;
	AST*         body;
};

struct AST_unop : public AST {
	OpType       op;
	AST*         operand;
};
struct AST_binop : public AST {
	OpType       op;
	AST*         lhs;
	AST*         rhs;
};

struct AST_return : public AST {
	OpType       op;
	size_t       argc;
	AST*         args;
};

// helper function to iterate all child AST nodes and call a func on them
template <typename FUNC>
inline void visit (AST* node, FUNC func) {
	assert(node);

	switch (node->type) {
		case A_BLOCK: { auto* block = (AST_block*)node;
			for (auto* n=block->statements; n != nullptr; n = n->next)
				func(n);
		} break;

		case A_FUNCDEF: { auto* f = (AST_funcdef*)node;
			for (auto* n=f->args; n != nullptr; n = n->next)
				func(n);
			for (auto* n=f->rets; n != nullptr; n = n->next)
				func(n);
			func(f->body);
		} break;

		case A_CALLARG: { auto* arg = (AST_callarg*)node;
			func(arg->expr);
		} break;

		case A_CALL: { auto* call = (AST_call*)node;
			for (auto* arg=call->args; arg != nullptr; arg = arg->next)
				func(arg);
		} break;
		case A_RETURN: { auto* ret = (AST_return*)node;
			for (auto* arg=ret->args; arg != nullptr; arg = arg->next)
				func(arg);
		} break;

		case A_IF:
		case A_SELECT: { auto* aif = (AST_if*)node;
			func(aif->cond      );
			func(aif->if_body );
			if (aif->else_body)
				func(aif->else_body);
		} break;

		case A_WHILE:
		case A_DO_WHILE:
		case A_FOR: { auto* loop = (AST_loop*)node;
			if (loop->start)
				func(loop->start);

			func(loop->cond );

			if (loop->end)
				func(loop->end);

			func(loop->body);
		} break;

		case A_UNOP: { auto* op = (AST_unop*)node;
			func(op->operand);
		} break;

		case A_BINOP:
		case A_ASSIGNOP: { auto* op = (AST_binop*)node;
			func(op->lhs);
			func(op->rhs);
		} break;

		default:
			return;
			//func(node);
	}
}
inline void dbg_print (AST* node, int depth=0) {
	if (!node) return;
	
	auto indent = [] (int depth) {
		for (int i=0; i<depth; ++i)
			printf("  ");
	};

	indent(depth);
	printf("%s", ASTType_str[node->type]);

	bool children = false;
	switch (node->type) {
		case A_LITERAL: { auto* lit = (AST_literal*)node;
			std::string str(lit->src_tok->source.text());
			printf(" %s\n", str.c_str());
		} break;

		case A_VARDECL: { auto* var = (AST_vardecl*)node;
			std::string str(var->ident);
			printf(" %s (%s)\n", str.c_str(), Type_str[var->valtype]);
		} break;

		case A_VAR: { auto* var = (AST_var*)node;
			std::string str(var->ident);
			printf(" %s\n", str.c_str());
		} break;

		case A_FUNCDEF: { auto* f = (AST_funcdef*)node;
			std::string str(f->ident);
			printf(" %s\n", str.c_str());

			depth++;

			if (f->args) {
				indent(depth); printf("args: (\n");
				visit((AST*)f->args, [=] (AST* node) { dbg_print(node, depth+1); });
				indent(depth); printf(")\n");
			} else {
				indent(depth); printf("args: ()\n");
			}

			if (f->rets) {
				indent(depth); printf("rets: (\n");
				visit((AST*)f->rets, [=] (AST* node) { dbg_print(node, depth+1); });
				indent(depth); printf(")\n");
			} else {
				indent(depth); printf("rets: ()\n");
			}

			indent(depth); printf("(\n");
			visit(f->body, [=] (AST* node) { dbg_print(node, depth+1); });
			indent(depth); printf(")\n");
		} break;

		case A_CALLARG: { auto* arg = (AST_callarg*)node;
			if (!arg->ident.empty()) {
				std::string str(arg->ident);
				printf(" %s=", str.c_str());
			}
			children = true;
		} break;
		case A_CALL: { auto* call = (AST_var*)node;
			std::string str(call->ident);
			printf(" %s", str.c_str());
			children = true;
		} break;
		
		case A_RETURN: {
			children = true;
		} break;
		case A_BREAK:
		case A_CONTINUE: {
			printf("\n");
		} break;

		case A_UNOP: { auto* op = (AST_unop*)node;
			printf("(%s)", OpType_str[op->op]);
			children = true;
		} break;

		case A_BINOP: 
		case A_ASSIGNOP: { auto* op = (AST_binop*)node;
			printf("(%s)", OpType_str[op->op]);
			children = true;
		} break;

		default:
			children = true;
	}

	if (children) {
		printf(" (\n");
		visit(node, [=] (AST* node) { dbg_print(node, depth+1); });
		indent(depth); printf(")\n");
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

	template <typename FUNC>
	size_t comma_seperated_list (AST** link, FUNC element, TokenType endtok) {
		*link = nullptr;

		size_t count = 0;
		while (tok->type != endtok) {
			*link = element();

			link = &(*link)->next;
			count++;

			if (tok->type == T_COMMA) {
				tok++;
			}
			else if (tok->type != endtok) {
				throw_error_after("syntax error: ',' or ')' expected!", tok[-1]); // TODO: use endtok
			}
		}
		return count;
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
					return (AST*)var;
				}
			}

			case T_LITERAL: {
				auto* lit = ast_alloc<AST_literal>(A_LITERAL, tok);
				lit->valtype = tok->lit_type;
				lit->value   = tok->lit_val;
				tok++;
				return (AST*)lit;
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
			lhs = (AST*)unary_op;
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
			lhs = (AST*)post_op;
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
				lhs = (AST*)op;
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

				lhs = (AST*)op;
			}
		}

		return lhs;
	}

	//    <varname> :              -> variable declaration with inferred type (must be followed by  = <expression>  to infer the type from)
	// or <varname> : <typename>   -> variable declaration with explicit type
	AST* var_decl () {
		if (!(tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON))
			throw_error("syntax error: expected variable declaration", *tok);

		auto* var = ast_alloc<AST_vardecl>(A_VARDECL, tok);
		var->ident = tok[0].source.text();

		tok+=2;

		// type specifier
		if (tok->type == T_IDENTIFIER) {
			auto ident = tok->source.text();

			if      (ident == "bool") var->valtype = BOOL;
			else if (ident == "int" ) var->valtype = INT;
			else if (ident == "flt" ) var->valtype = FLT;
			else if (ident == "str" ) var->valtype = STR;

			tok++;
		}
		else {
			var->valtype = VOID;

			if (tok->type != T_ASSIGN)
				throw_error_after("syntax error: \neither specify type during variable declaration with \"<var> : <type>;\"\n"
					                              "or let type be inferred with \"<var> := <expr>;\"", tok[-1]);
		}

		if (tok->type == T_ASSIGN) {
			tok++;
			var->init = expression(0);
		}
		return (AST*)var;
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
			return (AST*)op;
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
	AST* call_arg (TokenType endtok) {
		auto* arg = ast_alloc<AST_callarg>(A_CALLARG, tok);
		arg->ident = strview();

		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_ASSIGN) {
			arg->ident = tok[0].source.text();
			tok += 2;
		}

		arg->expr = expression(0);
		return (AST*)arg;
	}

	// (<call_arg>, <call_arg> etc.)
	size_t call_args (AST** args) {
		assert(tok->type == T_PAREN_OPEN);
		tok++;

		auto count = comma_seperated_list(args, [this] () {
			return call_arg(T_PAREN_CLOSE);
		}, T_PAREN_CLOSE);

		tok++; // T_PAREN_CLOSE
		return count;
	}

	// <funcname><call_args>
	AST* call () {
		auto* call = ast_alloc<AST_call>(A_CALL, tok);
		call->ident = tok->source.text();
		tok++;

		call->argc = call_args(&call->args);

		return (AST*)call;
	}

	// <call_arg>, <call_arg> etc.
	size_t return_args (AST** args) {

		auto count = comma_seperated_list(args, [this] () {
			return call_arg(T_SEMICOLON);
		}, T_SEMICOLON);

		return count;
	}

	// return <return_args>;
	AST* return_ () {
		auto* ast = ast_alloc<AST_return>(A_RETURN, tok++);

		ast->argc = return_args(&ast->args);

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

		return (AST*)aif;
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

		return (AST*)loop;
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

		return (AST*)loop;
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

		return (AST*)loop;
	}
	
	AST* _const_vardecl () {
		auto* decl = (AST_vardecl*)var_decl();

		if (decl->init) {
			// TOOD: implement at const-foldable expression for default args at the very least
			// better yet allow things like  sqrt(5)  or even custom compile-time const functions to be called as well
			// or just const values in general (const globals or const locally captured vars)
			// the question is where the const folding happens -> wait until I get to actually implementing compile-time execution
			if (decl->init->type != A_LITERAL)
				throw_error("syntax error: only literals allowed as default argument values (for now)", *decl->init->src_tok);

			// TODO: do this now to simply code during resolving (after funcdef prescan we need to know arg/ret types)
			//       later we will need a more structured way to handle dependencies between struct members/func args and their use sites
			decl->valtype = decl->init->valtype;
		}

		return (AST*)decl;
	}

	size_t fdef_arglist (AST** link) {
		assert(tok->type == T_PAREN_OPEN);
		tok++;

		auto count = comma_seperated_list(link, [this] () {
			return _const_vardecl();
		}, T_PAREN_CLOSE);

		tok++; // T_PAREN_CLOSE
		return count;
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

		func->argc = fdef_arglist(&func->args);

		// implicit (void) return list
		if (tok->type != T_ASSIGN) {
			func->retc = 0;
			func->rets = nullptr;
		}
		// explicit return list
		else {
			tok++;

			func->retc = fdef_arglist(&func->rets);
		}

		func->body = block();

		return (AST*)func;
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
				return return_();
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

	// {
	//   <statement>
	//   <statement>
	//   ...
	// }
	AST* block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error: '{' expected", *tok);

		auto* block = ast_alloc<AST_block>(A_BLOCK, tok++);

		block->statements = nullptr;

		AST** link = &block->statements;
		while (tok->type != T_BLOCK_CLOSE) {
			*link = statement();
			link = &(*link)->next;
		};

		//if (tok->type != T_BLOCK_CLOSE)
		//	throw_error("syntax error, '}' expected", *tok);
		tok++;
		return (AST*)block;
	}

	// <statement>
	// <statement>
	// ...
	AST* file () {
		auto* block = ast_alloc<AST_block>(A_BLOCK, tok);
		block->statements = nullptr;

		AST** link = &block->statements;
		while (tok->type != T_EOF) {
			AST* expr = statement();

			if (expr) {
				*link = expr;
				link = &(*link)->next;
			}
		}

		if (tok->type != T_EOF)
			throw_error("syntax error: end of file expected", *tok);

		return (AST*)block;
	}
};
