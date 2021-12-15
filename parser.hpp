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

void indent (int depth) {
	for (int i=0; i<depth; ++i)
		printf("  ");
}

template <typename T>
T* ast_alloc (ASTType type) {

#if USE_ALLOCATOR
	T* ret = g_allocator.alloc<T>();
	new (ret) T ();
#else
	T* ret = new T();
#endif

	ret->type = type;
	return ret;
}

struct AST_base;

#if USE_ALLOCATOR
	struct BumpAllocDeleter {
		template <typename T>
		inline void operator() (T* ptr) {
			// need to actually call dtor
			ptr->~T();
			// memory free is no-op
			// we the whole tree by reseting the bump allocator once we are done with the AST
		}
	};
	typedef std::unique_ptr<AST_base, BumpAllocDeleter> ast_ptr;
#else
	typedef std::unique_ptr<AST_base> ast_ptr;
#endif

struct AST_base {
	ASTType      type;
	source_range source;

	ast_ptr      next;

	virtual ~AST_base () {}
	virtual void dbg_print (int depth) {
		indent(depth);
		printf("%s ", ASTType_str[type]);
	}
};

struct AST_literal : public AST_base {
	Value        value;

	virtual ~AST_literal () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		std::string str(source.text());
		printf("%s\n", str.c_str());
	}
};
struct AST_var : public AST_base {
	strview      ident;
	size_t       addr;

	virtual ~AST_var () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		std::string str(ident);
		printf("%s\n", str.c_str());
	}
};
struct AST_unop : public AST_base {
	ast_ptr      operand;

	virtual ~AST_unop () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		printf("(\n");
		operand->dbg_print(depth+1);
		indent(depth); printf(")\n");
	}
};
struct AST_binop : public AST_base {
	ast_ptr      lhs;
	ast_ptr      rhs;

	virtual ~AST_binop () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		printf("(\n");
		lhs->dbg_print(depth+1);
		rhs->dbg_print(depth+1);
		indent(depth); printf(")\n");
	}
};
struct AST_call : public AST_base {
	strview      ident;

	size_t       argc;
	ast_ptr      args;

	virtual ~AST_call () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		std::string str(ident);
		printf("%s", str.c_str());

		printf("(\n");
		for (auto* n=args.get(); n != nullptr; n = n->next.get())
			n->dbg_print(depth+1);
		indent(depth); printf(")\n");
	}
};
struct AST_block : public AST_base {
	ast_ptr      statements;

	virtual ~AST_block () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		printf("(\n");
		for (auto* n=statements.get(); n != nullptr; n = n->next.get())
			n->dbg_print(depth+1);
		indent(depth); printf(")\n");
	}
};
struct AST_if : public AST_base {
	ast_ptr      cond;
	ast_ptr      true_body;
	ast_ptr      false_body;

	virtual ~AST_if () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		printf("(\n");
		cond      ->dbg_print(depth+1);
		true_body ->dbg_print(depth+1);
		false_body->dbg_print(depth+1);
		indent(depth); printf(")\n");
	}
};
struct AST_loop : public AST_base {
	ast_ptr      start;
	ast_ptr      cond;
	ast_ptr      body;
	ast_ptr      end;

	virtual ~AST_loop () {}
	virtual void dbg_print (int depth) {
		AST_base::dbg_print(depth);
		printf("(\n");
		start->dbg_print(depth+1);
		cond ->dbg_print(depth+1);
		body ->dbg_print(depth+1);
		end  ->dbg_print(depth+1);
		indent(depth); printf(")\n");
	}
};

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

					auto* call = ast_alloc<AST_call>(A_CALL);
					call->source.start = tok->source.start;
					call->ident = tok->source.text();

					tok+=2;

					call->argc = 0;

					ast_ptr* link = &call->args;
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

					call->source.end = tok->source.end;
					tok++;
					return ast_ptr(call);
				}
				// variable
				else {
					auto* var = ast_alloc<AST_var>(A_VAR);
					var->source = tok->source;
					var->ident = tok->source.text();

					tok++;
					return ast_ptr(var);
				}
			}

			case T_LITERAL: {
				auto* lit = ast_alloc<AST_literal>(A_LITERAL);
				lit->source = tok->source;
				lit->value = tok->val;
				tok++;
				return ast_ptr(lit);
			}
			
			default: {
				throw_error("syntax error, number or variable expected", *tok);
				return nullptr;
			}
		}
	}

	ast_ptr expression (unsigned min_prec) {

		// unary prefix operators
		if (tok->type == T_ADD)
			tok++; // unary plus is no-op

		ast_ptr lhs;

		if (is_unary_prefix_op(tok->type)) {
			auto* unary_op = ast_alloc<AST_unop>(tok2unop(tok->type));
			unary_op->source = tok->source;

			unsigned prec = un_prec(tok->type);
			tok++;

			unary_op->operand = expression(prec);
			lhs = ast_ptr(unary_op);
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
			post_op->source = tok->source;

			tok++;

			post_op->operand = std::move(lhs);
			lhs = ast_ptr(post_op);
		}

		// binary and ternary operators
		while (is_binary_or_ternary_op(tok->type)) {
			unsigned prec  = bt_prec( tok->type);
			unsigned assoc = bt_assoc(tok->type);

			if (prec < min_prec)
				break;

			Token& op_tok = *tok++;

			ast_ptr rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			if (op_tok.type == T_QUESTIONMARK) {
				if (tok->type != T_COLON)
					throw_error_after("syntax error, ':' expected after true case of select operator", tok[-1]);
				tok++;

				auto* op = ast_alloc<AST_if>(tok2btop(op_tok.type));
				op->cond       = std::move(lhs);
				op->true_body  = std::move(rhs);

				assert(assoc == RIGHT_ASSOC);
				op->false_body = expression(prec);

				lhs = ast_ptr(op);
			}
			else {
				auto* op = ast_alloc<AST_binop>(tok2btop(op_tok.type));
				op->source = op_tok.source;

				op->lhs = std::move(lhs);
				op->rhs = std::move(rhs);
				lhs = ast_ptr(op);
			}
		}

		return lhs;
	}

	ast_ptr assign_expr () {
		ast_ptr lhs;

		// lhs declaration in potential assignment
		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON) {
			auto* var = ast_alloc<AST_var>(A_VAR_DECL);
			var->source.start = tok[0].source.start;
			var->source.end   = tok[1].source.end;
			var->ident = tok[0].source.text();
			lhs = ast_ptr(var);

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
			op->source = tok->source;
			tok++;

			ast_ptr rhs = expression(0);

			op->lhs = std::move(lhs);
			op->rhs = std::move(rhs);
			lhs = ast_ptr(op);
		}

		return lhs;
	}

	ast_ptr for_loop () {
		Token* loop_tok = tok++;
		auto* loop = ast_alloc<AST_loop>(A_LOOP);

		loop->start = assign_expr();
		eat_semicolon();

		loop->cond  = expression(0);
		eat_semicolon();

		loop->end   = assign_expr();

		loop->source.start = loop_tok->source.start;
		loop->source.end   = tok[-1].source.end;

		loop->body = block();

		return ast_ptr(loop);
	}

	// parses  if <cond> {} elif <cond> {} elif <cond> else {}
	// where each elif and else is optional
	ast_ptr if_statement () {
		auto* aif = ast_alloc<AST_if>(A_IF); 
		aif->source.start = tok->source.start;
		tok++;

		aif->cond       = expression(0);

		aif->source.end = tok[-1].source.end;

		aif->true_body  = block();
		aif->false_body = elif_statement();

		return ast_ptr(aif);
	}
	ast_ptr elif_statement () {
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
				ast_ptr statement = assign_expr();
				eat_semicolon();
				return statement;
			}
		}
	}

	ast_ptr block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error, '{' expected", *tok);

		auto* block = ast_alloc<AST_block>(A_BLOCK);
		block->source = tok->source;

		tok++;

		ast_ptr* link = &block->statements;
		while (tok->type != T_BLOCK_CLOSE) {
			*link = statement();
			link = &(*link)->next;
		};

		//if (tok->type != T_BLOCK_CLOSE)
		//	throw_error("syntax error, '}' expected", *tok);
		tok++;
		return ast_ptr(block);
	}

	ast_ptr file () {
		ZoneScoped;

		auto* block = ast_alloc<AST_block>(A_BLOCK);
		block->source.start = tok[0].source.start;

		ast_ptr* link = &block->statements;
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
		return ast_ptr(block);
	}
};
