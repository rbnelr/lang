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

void println (Value& arg) {

	printf("%g\n", arg.val);
}

struct Interpreter {
	Tokenizer tok;

	void throw_error_after (const char* errstr, Token const& after_tok) {
		const char* end = after_tok.text.data() + after_tok.text.size();
		throw Exception{ errstr, end, end+1, after_tok.lineno };
	}
	void throw_error (const char* errstr, Token const& tok) {
		const char* end = tok.text.data() + tok.text.size();
		throw Exception{ errstr, tok.text.data(), end, tok.lineno };
	}
	void throw_error (const char* errstr, strview const& range, size_t lineno) {
		throw Exception{ errstr, range.data(), range.data() + range.size(), lineno };
	}

	Value call_function (strview const& name, Value* arg, size_t argc, strview const& range, size_t lineno) {
		if (name == "println") {
			if (argc != 1) throw_error("println takes 1 argument", range, lineno);
			println(arg[0]);
		}
		else {
			throw_error("unknown function", range, lineno);
		}
		return {};
	}

	std::unordered_map<strview, Value> variables;

	Value atom () {
		Value result;

		if (tok.eat(T_PAREN_OPEN)) {
			result = expression();

			if (!tok.eat(T_PAREN_CLOSE)) {
				throw_error_after("syntax error, ')' expected", tok.prev());
			}
		}
		else if (tok.peek(0) == T_IDENTIFIER && tok.peek(1) == T_PAREN_OPEN) {
			Token funcname = tok.get();
			tok.get(); // T_PAREN_OPEN
			
			std::vector<Value> args;
			args.reserve(16);
			
			while (!tok.eat(T_PAREN_CLOSE)) {
				args.emplace_back( expression() );
			
				if (!tok.eat(T_COMMA) && tok.peek() != T_PAREN_CLOSE) {
					throw_error_after("syntax error, ',' or ')' expected!", tok.prev());
				}
			}
			auto paren_close = tok.prev().text;

			strview range = strview(funcname.text.data(), paren_close.data() + paren_close.size() - funcname.text.data());
			result = call_function(funcname.text, args.data(), args.size(), range, funcname.lineno);
		}
		else if (tok.peek() == T_LITERAL_FLOAT) {
			result = { (float)tok.get().value_flt };
		}
		else if (tok.peek() == T_IDENTIFIER) {
			Token varname = tok.get();

			auto it = variables.find(varname.text);
			if (it == variables.end())
				throw_error("unknown variable", varname);

			result = it->second;
		}
		else {
			throw_error_after("syntax error, number or variable expected", tok.prev());
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
				Value result = { -lhs.val };

				lhs = result;
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
	
	void statements () {

		while (tok.peek() != T_EOF) {

			Value* lhs = nullptr;

			if (tok.peek(0) == T_IDENTIFIER && tok.peek(1) == T_EQUALS) {
				Token variable = tok.get();
				tok.get(); // T_EQUALS

				lhs = &variables[variable.text];
			}

			Value result = expression();
			if (lhs)
				*lhs = result;

			if (!tok.eat(T_SEMICOLON)) {
				throw_error_after("syntax error, ';' expected", tok.prev());
			}
		}

		//if (tok.peek() != T_EOF) {
		//	throw_error("syntax error, end of input expected", tok.buf[0]);
		//}
	}
};
