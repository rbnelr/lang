#pragma once
#include "tokenizer.hpp"
#include "errors.hpp"

inline constexpr bool is_binary_op (TokenType tok) {
	return tok >= T_PLUS && tok <= T_DIVIDE;
}

inline constexpr uint8_t BINARY_OP_PRECEDENCE[] = {
	0, // T_PLUS,
	0, // T_MINUS,
	1, // T_MULTIPLY,
	1, // T_DIVIDE,
	2, // T_POWER,
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

inline int get_binary_op_precedence (TokenType tok) {
	assert(is_binary_op(tok));
	return BINARY_OP_PRECEDENCE[tok - T_PLUS];
}
inline int get_binary_op_associativity (TokenType tok) {
	assert(is_binary_op(tok));
	return (bool)BINARY_OP_ASSOCIATIVITY[tok - T_PLUS];
}

struct Value {
	float val;
};

struct Interpreter {
	Tokenizer tok;

	void throw_error (const char* errstr, Token& tok) {
		throw Exception{ errstr, tok.text.data(), tok.text.data()+1, tok.lineno };
	}

	int depth = 0;

	Value atom () {
		Value result;

		if (tok.eat(T_PAREN_OPEN)) {
			result = expression();

			if (!tok.eat(T_PAREN_CLOSE)) {
				throw_error("syntax error, ')' expected", tok.buf[0]);
			}
		}
		else if (tok.peek(0) == T_IDENTIFIER && tok.peek(1) == T_PAREN_OPEN) {
			//// function call
			//result = ast_node(OP_FUNCCALL, tok.get());
			//tok.get(); // T_PAREN_OPEN
			//
			//ast_ptr* arg_ptr = &result->child;
			//
			//int argc = 0;
			//
			//if (tok.eat(T_PAREN_CLOSE)) {
			//	// 0 args
			//} else {
			//	for (;;) {
			//		*arg_ptr = expression();
			//		if (!*arg_ptr) return nullptr;
			//
			//		argc++;
			//		arg_ptr = &(*arg_ptr)->next;
			//
			//		if (tok.eat(T_COMMA)) {
			//			continue;
			//		} else if (tok.eat(T_PAREN_CLOSE)) {
			//			break;
			//		} else {
			//			last_err = "syntax error, ',' or ')' expected!";
			//			return nullptr;
			//		}
			//	}
			//}
			//
			//result->op.argc = argc;
			assert(false);
		}
		else if (tok.peek() == T_LITERAL_FLOAT) {
			result = { (float)tok.get().value_flt };
		}
		else if (tok.peek() == T_IDENTIFIER) {
			//type = OP_VARIABLE;
			assert(false);
		}
		else {
			throw_error("syntax error, number or variable expected", tok.buf[0]);
		}

		return result;
	}

	// a series of atoms seperated by binary operators (of precedence higher or equal than min_prec)
	// ex. -x^(y+3) + 5
	// note that the (y+3) is an atom, which happens to be a sub-expression
	// expression calls itself recursively with increasing min_precedences to generate operators in the correct order (precedence climbing algorithm)
	Value expression (int min_prec = 0) {

		bool unary_minus = false;
		int unary_prec = 0;

		if      (tok.eat(T_MINUS)) unary_minus = true;
		else if (tok.eat(T_PLUS) ) unary_minus = false; // unary plus is no-op

		Value lhs = atom();

		for (;;) {
			auto op_type = tok.peek();
			if (!is_binary_op(op_type))
				break;

			int  prec  = get_binary_op_precedence(   op_type);
			auto assoc = get_binary_op_associativity(op_type);

			if (prec < min_prec)
				break;

			if (unary_minus && unary_prec >= prec) {
				lhs.val = -lhs.val;
				unary_minus = false;
			}

			tok.get(); // eat operator
			
			Value rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			switch (op_type) {
				case T_PLUS     : lhs.val = lhs.val + rhs.val; break;
				case T_MINUS    : lhs.val = lhs.val - rhs.val; break;
				case T_MULTIPLY : lhs.val = lhs.val * rhs.val; break;
				case T_DIVIDE   : lhs.val = lhs.val / rhs.val; break;
				default: assert(false);
			}
		}

		if (unary_minus) {
			lhs.val = -lhs.val;
		}
		return lhs;
	}
	
	Value statement () {
		Value result = expression();

		if (!tok.eat(T_SEMICOLON)) {
			throw_error("syntax error, ';' expected", tok.buf[0]);
		}

		//if (tok.peek() == T_PAREN_CLOSE) {
		//	throw_error("syntax error, ')' without matching '('", tok.buf[0]);
		//}
		if (tok.peek() != T_EOF) {
			throw_error("syntax error, end of input expected", tok.buf[0]);
		}

		return result;
	}
};
