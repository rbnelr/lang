#pragma once
#include "common.hpp"
#include "tokenizer.hpp"
#include "errors.hpp"
#include "types.hpp"

inline constexpr bool is_binary_op (TokenType tok) {
	return tok >= T_ADD && tok <= T_NOT_EQUALS;
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
	2, // T_ADD
	2, // T_SUB
	4, // T_MUL
	4, // T_DIV

	1, // T_LESS
	1, // T_LESSEQ
	1, // T_GREATER
	1, // T_GREATEREQ
	0, // T_EQUALS
	0, // T_NOT_EQUALS
	
	255, // T_NOT
	255, // T_INC
	255, // T_DEC
};
inline constexpr uint8_t UNARY_OP_PRECEDENCE[] = {
	255, // T_ADD
	3,   // T_SUB
	255, // T_MUL
	255, // T_DIV

	255, // T_LESS
	255, // T_LESSEQ
	255, // T_GREATER
	255, // T_GREATEREQ
	255, // T_EQUALS
	255, // T_NOT_EQUALS
	
	5, // T_NOT
	6, // T_INC
	6, // T_DEC
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

	RIGHT_ASSOC, // T_NOT
	LEFT_ASSOC, // T_INC
	LEFT_ASSOC, // T_DEC
};

inline unsigned bin_prec (TokenType tok) {
	assert(is_binary_op(tok));
	return BINARY_OP_PRECEDENCE[tok - T_ADD];
}
inline unsigned un_prec (TokenType tok) {
	assert(is_unary_op(tok));
	return UNARY_OP_PRECEDENCE[tok - T_ADD];
}
inline unsigned bin_assoc (TokenType tok) {
	assert(is_binary_op(tok));
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
	A_LOOP,
	A_IF,

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

inline constexpr ASTType tok2binop (TokenType tok) {
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

struct AST;
typedef std::unique_ptr<AST> ast_ptr;

struct AST {
	ASTType      type;

	source_range source;

	ast_ptr      next;
	ast_ptr      child;

	union {
		struct {
			Value value; // for constants
		} literal;

		struct {
			strview   ident;
			size_t    addr;
		} var;

		struct {
			strview   ident;
			size_t    argc;
		} call;
	};

	AST () {}
	~AST () {
		if (type == A_LITERAL)
			literal.value.~Value();
	}
};

ast_ptr ast_alloc (ASTType type) {
	ast_ptr ret = std::make_unique<AST>();
	memset(ret.get(), 0, sizeof(AST)); // 0 is a valid empty state for all members
	ret->type = type;
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

	void eat_semicolon () {
		if (tok->type != T_SEMICOLON)
			throw_error_after("syntax error, ';' expected", tok[-1]);
		tok++;
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

					ast_ptr call = ast_alloc(A_CALL);
					call->source.start = tok->source.start;
					call->call.ident = tok->source.text();

					tok+=2;

					call->call.argc = 0;

					ast_ptr* link = &call->child;
					while (tok->type != T_PAREN_CLOSE) {
						*link = expression(0);
						link = &(*link)->next;

						call->call.argc++;

						if (tok->type == T_COMMA) {
							tok++;
						}
						else if (tok->type != T_PAREN_CLOSE) {
							throw_error_after("syntax error, ',' or ')' expected!", tok[-1]);
						}
					}

					call->source.end = tok->source.end;
					tok++;
					return call;
				}
				// variable
				else {
					ast_ptr var = ast_alloc(A_VAR);
					var->source = tok->source;
					var->var.ident = tok->source.text();

					tok++;
					return var;
				}
			}

			case T_LITERAL: {
				ast_ptr lit = ast_alloc(A_LITERAL);
				lit->source = tok->source;
				lit->literal.value = std::move(tok->val);
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

		// prefix unary operators
		if (tok->type == T_ADD)
			tok++; // unary plus is no-op

		ast_ptr lhs;

		if (is_unary_prefix_op(tok->type)) {
			ast_ptr unary_op = ast_alloc(tok2unop(tok->type));
			unary_op->source = tok->source;
			unsigned prec = un_prec(tok->type);
			tok++;

			lhs = expression(prec);

			unary_op->child = std::move(lhs);
			lhs = std::move(unary_op);
		}
		else {
			lhs = atom();
		}

		while (is_unary_postfix_op(tok->type)) {
			unsigned prec = un_prec(tok->type);
			if (prec < min_prec)
				return lhs;

			ast_ptr post_op = ast_alloc(tok2unop(tok->type));
			post_op->source = tok->source;

			tok++;

			post_op->child = std::move(lhs);
			lhs = std::move(post_op);
		}

		while (is_binary_op(tok->type)) {
			unsigned prec  = bin_prec( tok->type);
			unsigned assoc = bin_assoc(tok->type);

			if (prec < min_prec)
				break;

			ast_ptr op = ast_alloc(tok2binop(tok->type));
			op->source = tok->source;
			tok++;

			ast_ptr rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			lhs->next  = std::move(rhs);
			op-> child = std::move(lhs);
			lhs = std::move(op);
		}

		return lhs;
	}

	ast_ptr assign_expr () {
		ast_ptr lhs;

		// lhs declaration in potential assignment
		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON) {
			lhs = ast_alloc(A_VAR_DECL);
			lhs->source.start = tok[0].source.start;
			lhs->source.end   = tok[1].source.end;
			lhs->var.ident = tok[0].source.text();

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

			ast_ptr assign = ast_alloc(tok2assign(tok->type));
			assign->source = tok->source;
			tok++;

			ast_ptr rhs = expression(0);

			lhs->next = std::move(rhs);
			assign->child = std::move(lhs);
			lhs = std::move(assign);
		}

		return lhs;
	}

	ast_ptr for_loop () {
		Token* loop_tok = tok++;
		ast_ptr loop = ast_alloc(A_LOOP);

		ast_ptr start = assign_expr();
		eat_semicolon();

		ast_ptr cond  = assign_expr();
		eat_semicolon();

		ast_ptr step  = assign_expr();

		loop->source.start = loop_tok->source.start;
		loop->source.end   = tok[-1].source.end;

		ast_ptr body = block();

		step ->next = std::move(body);
		cond ->next = std::move(step);
		start->next = std::move(cond);

		loop ->child = std::move(start);
		return loop;
	}

	ast_ptr _elif () {
		if (tok->type == T_ELSE) {
			tok++;
			return block();
		}
		if (tok->type != T_ELIF) {
			return nullptr;
		}

		ast_ptr if_kw = ast_alloc(A_IF); 
		if_kw->source.start = tok->source.start;
		tok++;

		ast_ptr cond = assign_expr();
		if_kw->source.end = tok[-1].source.end;

		ast_ptr body = block();
		ast_ptr else_ = _elif();

		body ->next  = std::move(else_);
		cond ->next  = std::move(body);
		if_kw->child = std::move(cond);
		return if_kw;
	}
	// parses  if <cond> {} elif <cond> {} elif <cond> else {}
	// where each elif and else is optional
	ast_ptr if_elif_else () {
		ast_ptr if_kw = ast_alloc(A_IF);
		if_kw->source.start = tok->source.start;
		tok++;

		ast_ptr cond = assign_expr();
		if_kw->source.end = tok[-1].source.end;

		ast_ptr body = block();
		ast_ptr else_ = _elif();
		
		body ->next  = std::move(else_); // can be null if no elif or else
		cond ->next  = std::move(body);
		if_kw->child = std::move(cond);
		return if_kw;
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
			}

			// for loop
			case T_IF: {
				return if_elif_else();
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
				ast_ptr statement = assign_expr();
				eat_semicolon();
				return statement;
			}
		}
	}

	ast_ptr block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error, '{' expected", *tok);

		ast_ptr block = ast_alloc(A_BLOCK);
		block->source = tok->source;

		tok++;

		ast_ptr* link = &block->child;
		while (tok->type != T_BLOCK_CLOSE) {
			*link = statement();
			link = &(*link)->next;
		};

		//if (tok->type != T_BLOCK_CLOSE)
		//	throw_error("syntax error, '}' expected", *tok);
		tok++;
		return block;
	}

	ast_ptr file () {
		ZoneScoped;

		ast_ptr block = ast_alloc(A_BLOCK);
		block->source.start = tok[0].source.start;

		ast_ptr* link = &block->child;
		while (tok->type != T_EOF) {
			ast_ptr expr = statement();

			if (expr) {
				*link = std::move(expr);
				link = &(*link)->next;
			}
		}

		if (tok->type != T_EOF)
			throw_error("syntax error, end of input expected", *tok);

		block->source.end = tok->source.end;
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

	if (node->type == A_VAR || node->type == A_VAR_DECL || node->type == A_LITERAL) {
		std::string str(node->source.text());
		printf("%s\n", str.c_str());
		assert(!node->child);
		return;
	}
	else if (node->type == A_CALL) {
		std::string str(node->call.ident);
		printf("%s", str.c_str());
	}

	printf("(\n");
	for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
		dbg_print(n, depth+1);
	};

	indent(depth);
	printf(")\n");
}
