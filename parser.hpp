#pragma once
#include "common.hpp"
#include "tokenizer.hpp"
#include "errors.hpp"
#include "types.hpp"

inline constexpr bool is_binary_or_ternary_op (TokenType tok) {
	return tok >= T_ADD && tok <= T_QUESTIONMARK;
}
inline constexpr bool is_binary_assignemnt_op (TokenType tok) {
	return tok >= T_ASSIGN && tok <= T_DIVEQ;
}

inline constexpr bool is_unary_op (TokenType tok) {
	return (tok >= T_ADD && tok <= T_SUB) || (tok >= T_NOT && tok <= T_DEC);
}
inline constexpr bool is_unary_prefix_op (TokenType tok) {
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
	255, // T_ADD
	4,   // T_SUB
	255, // T_MUL
	255, // T_DIV

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

	LEFT_ASSOC, // T_LESS
	LEFT_ASSOC, // T_LESSEQ
	LEFT_ASSOC, // T_GREATER
	LEFT_ASSOC, // T_GREATEREQ
	LEFT_ASSOC, // T_EQUALS
	LEFT_ASSOC, // T_NOT_EQUALS

	RIGHT_ASSOC, // T_QUESTIONMARK

	RIGHT_ASSOC, // T_NOT
	LEFT_ASSOC, // T_INC
	LEFT_ASSOC, // T_DEC
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

	// values
	A_LITERAL,
	A_VAR,
	A_VAR_DECL,

	A_CALL,

	// flow control
	A_IF,
	A_LOOP,

	// binary operators
	A_ADD,
	A_SUB,
	A_MUL,
	A_DIV,

	A_LESS,
	A_LESSEQ,
	A_GREATER,
	A_GREATEREQ,
	A_EQUALS,
	A_NOT_EQUALS,

	// ternary operator
	A_SELECT,

	// unary operators
	A_NEGATE,
	A_NOT,
	A_INC,
	A_DEC,

	A_ASSIGN,
	A_ADDEQ,
	A_SUBEQ,
	A_MULEQ,
	A_DIVEQ,
};
inline const char* ASTType_str[] = {
	"A_BLOCK",

	"A_LITERAL",
	"A_VAR",
	"A_VAR_DECL",

	"A_CALL",

	"A_LOOP",
	"A_IF",

	"A_ADD",
	"A_SUB",
	"A_MUL",
	"A_DIV",

	"A_LESS",
	"A_LESSEQ",
	"A_GREATER",
	"A_GREATEREQ",
	"A_EQUALS",
	"A_NOT_EQUALS",

	"A_SELECT",

	"A_NEGATE",
	"A_NOT",
	"A_INC",
	"A_DEC",

	"A_ASSIGN",
	"A_ADDEQ",
	"A_SUBEQ",
	"A_MULEQ",
	"A_DIVEQ",
};

inline constexpr ASTType tok2btop (TokenType tok) {
	return (ASTType)( tok + (A_ADD - T_ADD) );
}
inline constexpr ASTType tok2unop (TokenType tok) {
	assert(tok != T_ADD);
	if (tok == T_SUB) return A_NEGATE;

	return (ASTType)( tok + (A_NOT - T_NOT) );
}
inline constexpr ASTType tok2assign (TokenType tok) {
	return (ASTType)( tok + (A_ASSIGN - T_ASSIGN) );
}

inline constexpr ASTType assignop2binop (ASTType type) {
	assert(type >= A_ADDEQ && type <= A_DIVEQ);
	return (ASTType)( type + (A_ADD - A_ADDEQ) );
}

template <typename T>
T* ast_alloc (ASTType type) {
	T* ret = g_allocator.alloc<T>();
	// leave uninitialized, we always initialize every member

	ret->a.type = type;
	return ret;
}

struct AST {
	ASTType      type;
	source_range source;

	AST*         next;
};

struct AST_literal { AST a;
	Value        value;
};
struct AST_var { AST a;
	strview  ident;
	size_t   addr;
};
struct AST_unop { AST a;
	AST* operand;
};
struct AST_binop { AST a;
	AST* lhs;
	AST* rhs;
};
struct AST_call { AST a;
	strview  ident;

	size_t   argc;
	AST*     args;
};
struct AST_block { AST a;
	AST* statements;
};
struct AST_if { AST a;
	AST* cond;
	AST* true_body;
	AST* false_body;
};
struct AST_loop { AST a;
	AST* start;
	AST* cond;
	AST* body;
	AST* end;
};

// helper function to iterate all child AST nodes and call a func on them
template <typename FUNC>
void visit (AST* node, FUNC func) {
	switch (node->type) {
		case A_NEGATE: case A_NOT:
		case A_INC: case A_DEC: { auto* op = (AST_unop*)node;
			func(op->operand);
		} break;

		case A_ADD: case A_SUB: case A_MUL: case A_DIV:
		case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
		case A_EQUALS: case A_NOT_EQUALS:
		case A_ASSIGN:
		case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: {
			auto* op = (AST_binop*)node;
			func(op->lhs);
			func(op->rhs);
		} break;

		case A_CALL: { auto* call = (AST_call*)node;
			for (auto* n=call->args; n != nullptr; n = n->next)
				func(n);
		} break;

		case A_BLOCK: { auto* block = (AST_block*)node;
			for (auto* n=block->statements; n != nullptr; n = n->next)
				func(n);
		} break;

		case A_IF:
		case A_SELECT: { auto* aif = (AST_if*)node;
			func(aif->cond      );
			func(aif->true_body );
			func(aif->false_body);
		} break;

		case A_LOOP: { auto* loop = (AST_loop*)node;
			func(loop->start);
			func(loop->cond );
			func(loop->body );
			func(loop->end  );
		} break;

		default:
			assert(false);
			_UNREACHABLE;
	}
}

void dbg_print (AST* node, int depth=0) {
	auto indent = [] (int depth) {
		for (int i=0; i<depth; ++i)
			printf("  ");
	};

	indent(depth);
	printf("%s ", ASTType_str[node->type]);

	switch (node->type) {
		case A_LITERAL: { auto* lit = (AST_literal*)node;
			std::string str(lit->a.source.text());
			printf("%s\n", str.c_str());
		} break;

		case A_VAR_DECL:
		case A_VAR: { auto* var = (AST_var*)node;
			std::string str(var->ident);
			printf("%s\n", str.c_str());
		} break;

		default: {
			printf("(\n");
			visit(node, [=] (AST* node) { dbg_print(node, depth+1); });
			indent(depth); printf(")\n");
		} break;
	}
}

struct Parser {
	Token* tok;

	void throw_error_after (const char* errstr, Token const& after_tok) {
		throw MyException{errstr, {after_tok.source.end, after_tok.source.end+1}};
	}
	void throw_error (const char* errstr, Token const& tok) {
		throw MyException{errstr, tok.source };
	}
	void throw_error (const char* errstr, Token const& first, Token const& last) {
		throw MyException{errstr, {first.source.start, last.source.end} };
	}

	void eat_semicolon () {
		if (tok->type != T_SEMICOLON)
			throw_error_after("syntax error, ';' expected", tok[-1]);
		tok++;
	}

	AST* atom () {
		switch (tok->type) {

			case T_PAREN_OPEN: {
				// expression in parentheses
				tok++;

				AST* result = expression(0);

				if (tok->type != T_PAREN_CLOSE)
					throw_error_after("syntax error, parenthesis '(' not closed", tok[-1]);
				tok++;

				return result;
			}

			case T_IDENTIFIER: {
				// func call
				if (tok[1].type == T_PAREN_OPEN) {

					auto* call = ast_alloc<AST_call>(A_CALL);
					call->a.source.start = tok->source.start;
					call->ident = tok->source.text();

					tok+=2;

					call->argc = 0;

					AST** link = &call->args;
					while (tok->type != T_PAREN_CLOSE) {
						*link = expression(0);
						link = &(*link)->next;

						call->argc++;

						if (tok->type == T_COMMA) {
							tok++;
						}
						else if (tok->type != T_PAREN_CLOSE) {
							throw_error_after("syntax error, ',' or ')' expected!", tok[-1]);
						}
					}

					call->a.source.end = tok->source.end;
					tok++;
					return (AST*)call;
				}
				// variable
				else {
					auto* var = ast_alloc<AST_var>(A_VAR);
					var->a.source = tok->source;
					var->ident = tok->source.text();

					tok++;
					return (AST*)var;
				}
			}

			case T_LITERAL: {
				auto* lit = ast_alloc<AST_literal>(A_LITERAL);
				lit->a.source = tok->source;
				lit->value = tok->val;
				tok++;
				return (AST*)lit;
			}
			
			default: {
				throw_error("syntax error, number or variable expected", *tok);
				return nullptr;
			}
		}
	}

	AST* expression (unsigned min_prec) {

		// unary prefix operators
		if (tok->type == T_ADD)
			tok++; // unary plus is no-op

		AST* lhs;

		if (is_unary_prefix_op(tok->type)) {
			auto* unary_op = ast_alloc<AST_unop>(tok2unop(tok->type));
			unary_op->a.source = tok->source;

			unsigned prec = un_prec(tok->type);
			tok++;

			unary_op->operand = expression(prec);
			lhs = (AST*)unary_op;
		}
		else {
			lhs = atom();
		}

		// unary postfix operators
		while (is_unary_postfix_op(tok->type)) {
			unsigned prec = un_prec(tok->type);
			if (prec < min_prec)
				return lhs;

			auto* post_op = ast_alloc<AST_unop>(tok2unop(tok->type));
			post_op->a.source = tok->source;

			tok++;

			post_op->operand = lhs;
			lhs = (AST*)post_op;
		}

		// binary and ternary operators
		while (is_binary_or_ternary_op(tok->type)) {
			unsigned prec  = bt_prec( tok->type);
			unsigned assoc = bt_assoc(tok->type);

			if (prec < min_prec)
				break;

			Token& op_tok = *tok++;

			AST* rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			if (op_tok.type == T_QUESTIONMARK) {
				if (tok->type != T_COLON)
					throw_error_after("syntax error, ':' expected after true case of select operator", tok[-1]);
				tok++;

				auto* op = ast_alloc<AST_if>(tok2btop(op_tok.type));
				op->cond       = lhs;
				op->true_body  = rhs;

				assert(assoc == RIGHT_ASSOC);
				op->false_body = expression(prec);

				lhs = (AST*)op;
			}
			else {
				auto* op = ast_alloc<AST_binop>(tok2btop(op_tok.type));
				op->a.source = op_tok.source;

				op->lhs = lhs;
				op->rhs = rhs;
				lhs = (AST*)op;
			}
		}

		return lhs;
	}

	AST* assign_expr () {
		AST* lhs;

		// lhs declaration in potential assignment
		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON) {
			auto* var = ast_alloc<AST_var>(A_VAR_DECL);
			var->a.source.start = tok[0].source.start;
			var->a.source.end   = tok[1].source.end;
			var->ident = tok[0].source.text();
			lhs = (AST*)var;

			tok+=2;
		}
		// lhs expression in potential assignment
		else {
			lhs = expression(0);
		}

		// lhs = rhs   or  lhs += rhs  etc.
		if (is_binary_assignemnt_op(tok->type)) {
			if (lhs->type == A_VAR_DECL && tok->type != T_ASSIGN)
				throw_error("syntax error, cannot modify variable during declaration", *tok);

			auto* op = ast_alloc<AST_binop>(tok2assign(tok->type));
			op->a.source = tok->source;
			tok++;

			AST* rhs = expression(0);

			op->lhs = lhs;
			op->rhs = rhs;
			lhs = (AST*)op;
		}

		return lhs;
	}

	AST* for_loop () {
		Token* loop_tok = tok++;
		auto* loop = ast_alloc<AST_loop>(A_LOOP);

		loop->start = assign_expr();
		eat_semicolon();

		loop->cond  = expression(0);
		eat_semicolon();

		loop->end   = assign_expr();

		loop->a.source.start = loop_tok->source.start;
		loop->a.source.end   = tok[-1].source.end;

		loop->body = block();

		return (AST*)loop;
	}

	// parses  if <cond> {} elif <cond> {} elif <cond> else {}
	// where each elif and else is optional
	AST* if_statement () {
		auto* aif = ast_alloc<AST_if>(A_IF); 
		aif->a.source.start = tok->source.start;
		tok++;

		aif->cond       = expression(0);

		aif->a.source.end = tok[-1].source.end;

		aif->true_body  = block();
		aif->false_body = elif_statement();

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

	AST* statement () {

		auto eat_semicolon = [this] () {
			if (tok->type != T_SEMICOLON)
				throw_error_after("syntax error, ';' expected", tok[-1]);
			tok++;
		};

		switch (tok[0].type) {

			// block
			case T_BLOCK_OPEN: {
				return block();
			}

			// for loop
			case T_IF: {
				return if_statement();
			}

			// for loop
			case T_FOR: {
				return for_loop();
			}

			// allow empty statements
			case T_SEMICOLON: {
				tok++;
				return nullptr; // no-op
			}

			default: {
				AST* statement = assign_expr();
				eat_semicolon();
				return statement;
			}
		}
	}

	AST* block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error, '{' expected", *tok);

		auto* block = ast_alloc<AST_block>(A_BLOCK);
		block->a.source = tok->source;

		tok++;

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

	AST* file () {
		ZoneScoped;

		auto* block = ast_alloc<AST_block>(A_BLOCK);
		block->a.source.start = tok[0].source.start;

		AST** link = &block->statements;
		while (tok->type != T_EOF) {
			AST* expr = statement();

			if (expr) {
				*link = expr;
				link = &(*link)->next;
			}
		}

		if (tok->type != T_EOF)
			throw_error("syntax error, end of input expected", *tok);

		block->a.source.end = tok->source.end;
		return (AST*)block;
	}
};
