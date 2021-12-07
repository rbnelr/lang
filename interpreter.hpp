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

#undef NULL

enum Type {
	NULL=0,
	INT,
	FLT,
	STR,
};

#include <variant>
using _void = std::monostate;
typedef std::variant<_void, int64_t, double, std::string> Value;

#define NULLVAL Value{}

void print_val (Value const& arg) {
	switch ((Type)arg.index()) {
		case NULL:
			printf("null");
			break;
		case INT:
			printf("%" PRIi64, std::get<int64_t>(arg));
			break;
		case FLT:
			printf("%f", std::get<double>(arg));
			break;
		case STR:
			printf("%s", std::get<std::string>(arg).c_str());
			break;
		default:
			assert(false);
	}
}
void println (Value& val) {
	print_val(val);
	printf("\n");
}

void my_printf (Value& format, Value* args, size_t argc) {
	const char* cur = std::get<std::string>(format).c_str();

	size_t i = 0;
	while (*cur != '\0') {
		if (*cur == '%') {
			cur++;
			if (*cur != '%')
				print_val(i >= argc ? NULLVAL : args[i++]);
		}
		putc(*cur++, stdout);
	}
}

struct Interpreter {
	Token* tok;

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

	std::string parse_escaped_string (Token& tok) {
		assert(tok.type == T_LITERAL_STRING);

		const char* cur = tok.value_str.data();
		const char* end = tok.value_str.data() + tok.value_str.size();

		std::string result;
		result.resize(tok.value_str.size()); // resulting strings should be shorter than escaped strings
		char* out = result.data();

		while (cur < end) {
			if (*cur == '\\') {
				auto start = cur++;
				switch (*cur++) {
					case '0': *out++ = '\0'; break;
					case 'n': *out++ = '\n'; break;
					case 'r': *out++ = '\r'; break;
					default:
						throw_error("invalid escape sequence in literal string", strview(start, 2), tok.lineno);
				}
			} else {
				*out++ = *cur++;
			}
		}

		result.resize(out - result.data()); // resize to (smaller) real size
		return result;
	}

	Value binop (Value& lhs, Value& rhs, Token& op) {
		if (lhs.index() != rhs.index()) {
			throw_error("types do not match", op);
			return {};
		}
		switch ((Type)lhs.index()) {

			case INT:
				switch (op.type) {
					case T_PLUS     : return std::get<int64_t>(lhs) + std::get<int64_t>(rhs);
					case T_MINUS    : return std::get<int64_t>(lhs) - std::get<int64_t>(rhs);
					case T_MULTIPLY : return std::get<int64_t>(lhs) * std::get<int64_t>(rhs);
					case T_DIVIDE   : return std::get<int64_t>(lhs) / std::get<int64_t>(rhs);
					default: assert(false); return {};
				}
			case FLT:
				switch (op.type) {
					case T_PLUS     : return std::get<double>(lhs) + std::get<double>(rhs);
					case T_MINUS    : return std::get<double>(lhs) - std::get<double>(rhs);
					case T_MULTIPLY : return std::get<double>(lhs) * std::get<double>(rhs);
					case T_DIVIDE   : return std::get<double>(lhs) / std::get<double>(rhs);
					default: assert(false); return {};
				}

			case NULL: throw_error("can't do math with null", op);
			case STR:  throw_error("can't do math with str", op);
			default: assert(false); return {};
		}
	}

	Value negate (Value& rhs, Token& op) {
		switch ((Type)rhs.index()) {
			case NULL: throw_error("can't do math with null", op);
			case INT:  return -std::get<int64_t>(rhs);
			case FLT:  return -std::get<double >(rhs);
			case STR:  throw_error("can't do math with str", op);
			default: assert(false); return {};
		}
	}

	Value call_function (strview const& name, Value* args, size_t argc, strview const& range, size_t lineno) {
		auto match = [] (Value* args, size_t argc, std::initializer_list<Type> types, bool follow_vararg=false) {
			if (follow_vararg) {
				if (argc < types.size()) return false;
			} else {
				if (argc != types.size()) return false;
			}
			for (size_t i=0; i<types.size(); ++i) {
				if ((Type)args[i].index() != *(types.begin() + i)) return false;
			}
			return true;
		};

		if (name == "print") {
			if (argc != 1) goto mismatch;
			print_val(args[0]);
		}
		else if (name == "println") {
			if (argc != 1) goto mismatch;
			println(args[0]);
		}
		else if (name == "printf") {
			if (!match(args, argc, { STR }, true)) goto mismatch;
			my_printf(args[0], args+1, argc-1);
		}
		else {
			throw_error("unknown function", range, lineno);
		}

		return NULLVAL;

	mismatch:
		throw_error("no matching function overload", range, lineno);
		return {};
	}

	std::unordered_map<strview, Value> variables;

	Value atom () {
		switch (tok->type) {

			case T_PAREN_OPEN: {
				// expression in parentheses
				tok++;

				Value result = expression();

				if (tok->type != T_PAREN_CLOSE) {
					throw_error_after("syntax error, ')' expected", *tok);
				}
				tok++;

				return result;
			}

			case T_IDENTIFIER: {
				Token& ident = *tok++;

				if (tok->type == T_PAREN_OPEN) {
					// function call
					tok++; // T_PAREN_OPEN

					std::vector<Value> args;
					args.reserve(16);

					while (tok->type != T_PAREN_CLOSE) {
						args.emplace_back( expression() );

						if (tok->type == T_COMMA) {
							tok++;
						}
						else if (tok->type != T_PAREN_CLOSE) {
							throw_error_after("syntax error, ',' or ')' expected!", *tok);
						}
					}
					Token& paren_close = *tok++;

					strview range = strview(ident.text.data(), paren_close.text.data() + paren_close.text.size() - ident.text.data());
					return call_function(ident.text, args.data(), args.size(), range, ident.lineno);
				} else {
					// variable
					auto it = variables.find(ident.text);
					if (it == variables.end())
						throw_error("unknown variable", ident);

					return it->second;
				}
			}

			case T_LITERAL_INT:    return (*tok++).value_int;
			case T_LITERAL_FLOAT:  return (*tok++).value_flt;
			case T_LITERAL_STRING: return parse_escaped_string(*tok++);

			default: {
				throw_error_after("syntax error, number or variable expected", *tok);
				return {};
			}
		}
	}

	// a series of atoms seperated by binary operators (of precedence higher or equal than min_prec)
	// ex. -x^(y+3) + 5
	// note that the (y+3) is an atom, which happens to be a sub-expression
	// expression calls itself recursively with increasing min_precedences to generate operators in the correct order (precedence climbing algorithm)
	Value expression (int min_prec = 0) {

		Token* unary_minus = nullptr;
		int unary_prec = 0;

		if      (tok->type == T_MINUS) {
			unary_minus = tok++;
		}
		else if (tok->type == T_PLUS) {
			tok++; // unary plus is no-op
		}

		Value lhs = atom();

		for (;;) {
			TokenType op_type = tok->type;
			if (!is_binary_op(op_type))
				break;

			int prec  = get_binary_op_precedence(   op_type);
			int assoc = get_binary_op_associativity(op_type);

			if (prec < min_prec)
				break;

			if (unary_minus && unary_prec >= prec) {
				lhs = negate(lhs, *unary_minus);
				unary_minus = nullptr;
			}

			Token& op = *tok++; // eat operator

			Value rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			lhs = binop(lhs, rhs, op);
		}

		if (unary_minus)
			lhs = negate(lhs, *unary_minus);
		return lhs;
	}
	
	void statements () {

		while (tok->type != T_EOF) {

			Value* lhs = nullptr;

			if (tok[0].type == T_IDENTIFIER && tok[1].type == T_EQUALS) {
				Token& variable = *tok++;
				tok++; // T_EQUALS

				lhs = &variables[variable.text];
			}

			Value result = expression();
			if (lhs)
				*lhs = result;

			if (tok->type != T_SEMICOLON) {
				throw_error_after("syntax error, ';' expected", *tok);
			}
			tok++;
		}

		//if (tok.peek() != T_EOF) {
		//	throw_error("syntax error, end of input expected", tok.buf[0]);
		//}
	}
};
