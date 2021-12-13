#pragma once
#include "common.hpp"
#include "tokenizer.hpp"
#include "errors.hpp"
#include "types.hpp"

inline constexpr bool is_binary_op (TokenType tok) {
	return tok >= T_PLUS && tok <= T_NOT_EQUALS;
}

inline constexpr uint8_t BINARY_OP_PRECEDENCE[] = {
	2, // T_PLUS
	2, // T_MINUS
	3, // T_MULTIPLY
	3, // T_DIVIDE

	1, // T_LESS
	1, // T_LESSEQ
	1, // T_GREATER
	1, // T_GREATEREQ
	0, // T_EQUALS
	0, // T_NOT_EQUALS
	
	255, // T_NOT
};
inline constexpr uint8_t UNARY_OP_PRECEDENCE[] = {
	255, // T_PLUS
	3, // T_MINUS
	255, // T_MULTIPLY
	255, // T_DIVIDE

	255, // T_LESS
	255, // T_LESSEQ
	255, // T_GREATER
	255, // T_GREATEREQ
	255, // T_EQUALS
	255, // T_NOT_EQUALS
	
	4, // T_NOT
};

enum Associativity : uint8_t {
	LEFT_ASSOC=0,
	RIGHT_ASSOC=1,
};
inline constexpr Associativity BINARY_OP_ASSOCIATIVITY[] = { // 0 = left (left to right execution)  1 = right
	LEFT_ASSOC, // T_PLUS,
	LEFT_ASSOC, // T_MINUS,
	LEFT_ASSOC, // T_MULTIPLY,
	LEFT_ASSOC, // T_DIVIDE,
	RIGHT_ASSOC, // T_POWER,
};

inline unsigned get_binary_op_precedence (TokenType tok) {
	assert(is_binary_op(tok));
	return BINARY_OP_PRECEDENCE[tok - T_PLUS];
}
inline unsigned get_binary_op_associativity (TokenType tok) {
	assert(is_binary_op(tok));
	return (bool)BINARY_OP_ASSOCIATIVITY[tok - T_PLUS];
}
inline unsigned get_unary_op_precedence (TokenType tok) {
	assert(tok == T_MINUS || tok == T_NOT);
	return UNARY_OP_PRECEDENCE[tok - T_PLUS];
}

enum ASTType {
	A_BLOCK,

	// values
	A_CONSTANT,
	A_VARIABLE,

	A_ASSIGNMENT,

	A_CALL,

	// flow control
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

	// unary operators
	A_NOT,
	A_NEGATE,
};
inline const char* ASTType_str[] = {
	"A_BLOCK",

	"A_CONSTANT",
	"A_VARIABLE",

	"A_ASSIGNMENT",

	"A_CALL",

	"A_LOOP",

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

	"A_NOT",
	"A_NEGATE",
};

inline constexpr ASTType ASTType_from_TokenType (TokenType tok) {
	return (ASTType)( tok + (A_ADD - T_PLUS) );
}

struct AST;
typedef std::unique_ptr<AST> ast_ptr;

struct AST {
	ASTType      type;

	source_range source;

	Value        value; // for constants

	ast_ptr      next;
	ast_ptr      child;

	AST () {}
	~AST () {}

	void set_text_end_after (Token& endtok) {
		source.end = endtok.source.end;
	}
};

ast_ptr ast_alloc (ASTType type, Token& tok) {
	ast_ptr ret = std::make_unique<AST>();
	memset(ret.get(), 0, sizeof(AST)); // 0 is a valid empty state for all members
	ret->type   = type;
	ret->source = tok.source;
	return ret;
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

	ast_ptr atom () {
		switch (tok->type) {

			case T_PAREN_OPEN: {
				// expression in parentheses
				tok++;

				ast_ptr result = expression(0);

				if (tok->type != T_PAREN_CLOSE)
					throw_error_after("syntax error, parenthesis '(' not closed", tok[-1]);
				tok++;

				return result;
			}

			case T_IDENTIFIER: {
				// func call
				if (tok[1].type == T_PAREN_OPEN) {
					ast_ptr call = ast_alloc(A_CALL, *tok++);
					tok++; // T_PAREN_OPEN

					ast_ptr* pprev = &call->child;

					while (tok->type != T_PAREN_CLOSE) {
						*pprev = expression(0);
						pprev = &(*pprev)->next;

						if (tok->type == T_COMMA) {
							tok++;
						}
						else if (tok->type != T_PAREN_CLOSE) {
							throw_error_after("syntax error, ',' or ')' expected!", tok[-1]);
						}
					}

					//call->set_text_end_after(*tok++);
					tok++;
					return call;
				}
				// variable
				else {
					return ast_alloc(A_VARIABLE, *tok++);
				}
			}

			case T_LITERAL: {
				ast_ptr lit = ast_alloc(A_CONSTANT, *tok);
				lit->value = std::move(tok->val);
				tok++;
				return lit;
			}
			
			default: {
				throw_error("syntax error, number or variable expected", *tok);
				return nullptr;
			}
		}
	}

	ast_ptr expression (unsigned min_prec) {

		ast_ptr unary_op = nullptr;
		unsigned unary_prec;

		switch (tok->type) {
			case T_MINUS:
				unary_op = ast_alloc(A_NEGATE, *tok);
				unary_prec = get_unary_op_precedence((*tok++).type);
				min_prec = std::min(min_prec, unary_prec);
				break;
			case T_NOT:
				unary_op = ast_alloc(A_NEGATE, *tok++);
				unary_prec = get_unary_op_precedence((*tok++).type);
				min_prec = std::min(min_prec, unary_prec);
				break;
			case T_PLUS:
				tok++; // unary plus is no-op
				break;
		}

		ast_ptr lhs = atom();

		if (unary_op && is_binary_op(tok->type) && unary_prec >= get_binary_op_associativity(tok->type)) {
			unary_op->child = std::move(lhs);
			lhs = std::move(unary_op);
		}

		for (;;) {
			TokenType op_type = tok->type;
			if (!is_binary_op(op_type))
				break;

			unsigned prec  = get_binary_op_precedence(   op_type);
			unsigned assoc = get_binary_op_associativity(op_type);

			if (prec < min_prec)
				break;

			ast_ptr op = ast_alloc(ASTType_from_TokenType(tok->type), *tok);
			tok++;

			ast_ptr rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			lhs->next  = std::move(rhs);
			op-> child = std::move(lhs);
			lhs = std::move(op);
		}

		if (unary_op) {
			unary_op->child = std::move(lhs);
			lhs = std::move(unary_op);
		}
		return lhs;
	}

	ast_ptr expression_or_assignment () { // without semicolon
		ast_ptr expr = expression(0);

		// expression (not being assigned)
		if (tok->type != T_ASSIGN) {
			return expr;
		}
		// assignment
		else {
			ast_ptr assign = ast_alloc(A_ASSIGNMENT, *tok++);
			ast_ptr rhs = expression(0);

			expr->next    = std::move(rhs);
			assign->child = std::move(expr);

			return assign;
		}
	}

	ast_ptr statement () {

		auto eat_semicolon = [this] () {
			if (tok->type != T_SEMICOLON)
				throw_error_after("syntax error, ';' expected", tok[-1]);
			tok++;
		};

		switch (tok[0].type) {

			// block
			case T_BLOCK_OPEN: {
				return block();
			} break;

			// for loop
			case T_FOR: {
				ast_ptr loop = ast_alloc(A_LOOP, *tok++);

				ast_ptr start = expression_or_assignment();
				eat_semicolon();

				ast_ptr cond  = expression(0);
				eat_semicolon();

				ast_ptr step  = expression_or_assignment();

				ast_ptr body  = block();

				step ->next = std::move(body);
				cond ->next = std::move(step);
				start->next = std::move(cond);

				loop ->child   = std::move(start);

				loop->set_text_end_after(tok[-1]); // for - }
				return loop;
			} break;

			// allow empty statements
			case T_SEMICOLON: {
				tok++;
				return nullptr; // no-op
			}

			default: {
				ast_ptr statement = expression_or_assignment();
				eat_semicolon();
				return statement;

			}
		}
	}

	ast_ptr block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error, '{' expected", *tok);

		ast_ptr block = ast_alloc(A_BLOCK, *tok++);
		ast_ptr* prev = &block->child;

		while (tok->type != T_BLOCK_CLOSE) {
			*prev = statement();
			prev = &(*prev)->next;
		};

		//if (tok->type != T_BLOCK_CLOSE)
		//	throw_error("syntax error, '}' expected", *tok);
		tok++;

		block->set_text_end_after(tok[-1]);
		return block;
	}

	ast_ptr file () {
		ast_ptr block = ast_alloc(A_BLOCK, tok[0]);
		ast_ptr* prev = &block->child;

		while (tok->type != T_EOF) {
			ast_ptr expr = statement();

			if (expr) {
				*prev = std::move(expr);
				prev = &(*prev)->next;
			}
		}

		//if (tok->type != T_EOF)
		//	throw_error("syntax error, end of input expected", *tok);

		block->set_text_end_after(*tok);
		return block;
	}
};

void indent (int depth) {
	for (int i=0; i<depth; ++i)
		printf("  ");
}
void dbg_print (AST* node, int depth=0) {
	indent(depth);
	printf("%s ", ASTType_str[node->type]);

	if (node->type == A_CONSTANT) {
		print_val(node->value);
		printf("\n");
		assert(!node->child);
		return;
	}
	else if (node->type == A_VARIABLE) {
		std::string str(node->source.text());
		printf("%s\n", str.c_str());
		assert(!node->child);
		return;
	}
	else if (node->type == A_CALL || node->type == A_VARIABLE) {
		std::string str(node->source.text());
		printf("%s", str.c_str());
	}

	printf("(\n");
	for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
		dbg_print(n, depth+1);
	};

	indent(depth);
	printf(")\n");
}
