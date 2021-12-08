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

inline unsigned get_binary_op_precedence (TokenType tok) {
	assert(is_binary_op(tok));
	return BINARY_OP_PRECEDENCE[tok - T_PLUS];
}
inline unsigned get_binary_op_associativity (TokenType tok) {
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
struct Value {
	Type type;
	union {
		int64_t     i;
		double      f;
		std::string str;
	};

	~Value () {
		if (type == STR)
			str.~basic_string();
	}

	static void _copy (Value& l, Value const& r) {
		if (l.type == STR)
			l.str.~basic_string();
		l.type = NULL;

		if (r.type != STR) {
			memcpy(&l, &r, sizeof(Value));
		} else {
			l.type = r.type;

			new (&l.str) decltype(l.str) ();
			l.str = r.str;
		}
	}
	Value& operator= (Value const& v) {
		_copy(*this, v);
		return *this;
	}
	Value (Value const& v) {
		memset(this, 0, sizeof(Value));
		_copy(*this, v);
	}

	static void _move (Value& l, Value& r) {
		if (l.type == STR)
			l.str.~basic_string();
		l.type = NULL;

		memcpy(&l, &r, sizeof(Value));
		memset(&r, 0, sizeof(Value));
	}
	Value& operator= (Value&& v) {
		_move(*this, v);
		return *this;
	}
	Value (Value&& v) {
		memset(this, 0, sizeof(Value));
		_move(*this, v);
	}

	Value ()  {
		memset(this, 0, sizeof(Value));
	}
	Value (int64_t         i): type{INT},   i{i} {}
	Value (double          f): type{FLT},   f{f} {}
	Value (std::string&& str): type{STR}, str{std::move(str)} { }
};

void print_val (Value const& arg) {
	switch (arg.type) {
		case NULL:
			printf("null");
			break;
		case INT:
			printf("%" PRIi64, arg.i);
			break;
		case FLT:
			printf("%f", arg.f);
			break;
		case STR:
			printf("%s", arg.str.c_str());
			break;
		default:
			assert(false);
	}
}
void println (Value& val) {
	print_val(val);
	printf("\n");
}

void my_printf (Value& format, Value const* args, size_t argc) {
	const char* cur = format.str.c_str();

	size_t i = 0;
	while (*cur != '\0') {
		if (*cur == '%') {
			cur++;
			if (*cur != '%') {
				if (i >= argc) print_val(Value{});
				else           print_val(args[i++]);
			}
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
		throw Exception{ errstr, tok.text.data(), tok.text.data() + tok.text.size(), tok.lineno };
	}
	void throw_error (const char* errstr, Token const& first, Token const& last) {
		throw Exception{ errstr, first.text.data(), last.text.data() + last.text.size(), first.lineno };
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
		if (lhs.type != rhs.type) {
			throw_error("types do not match", op);
			return {};
		}
		switch (lhs.type) {
			case INT:
				switch (op.type) {
					case T_PLUS     : return { lhs.i + rhs.i };
					case T_MINUS    : return { lhs.i - rhs.i };
					case T_MULTIPLY : return { lhs.i * rhs.i };
					case T_DIVIDE   : return { lhs.i / rhs.i };
					default: assert(false); return {};
				}
			case FLT:
				switch (op.type) {
					case T_PLUS     : return { lhs.f + rhs.f };
					case T_MINUS    : return { lhs.f - rhs.f };
					case T_MULTIPLY : return { lhs.f * rhs.f };
					case T_DIVIDE   : return { lhs.f / rhs.f };
					default: assert(false); return {};
				}

			case NULL: throw_error("can't do math with null", op);
			case STR:  throw_error("can't do math with str", op);
			default: assert(false); return {};
		}
	}
	Value negate (Value& rhs, Token& op) {
		switch (rhs.type) {
			case NULL: throw_error("can't do math with null", op);
			case INT:  return -rhs.i;
			case FLT:  return -rhs.f;
			case STR:  throw_error("can't do math with str", op);
			default: assert(false); return {};
		}
	}

	Value call_function (strview const& name, Value* args, size_t argc, Token const& first, Token const& last) {
		auto match = [] (Value* args, size_t argc, std::initializer_list<Type> types, bool follow_vararg=false) {
			if (follow_vararg) {
				if (argc < types.size()) return false;
			} else {
				if (argc != types.size()) return false;
			}
			for (size_t i=0; i<types.size(); ++i) {
				if (args[i].type != *(types.begin() + i)) return false;
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
			throw_error("unknown function", first, last);
		}

		return {};

	mismatch:
		throw_error("no matching function overload", first, last);
		return {};
	}


	typedef std::unordered_map<strview, Value> Vars;
	Vars vars;

	Value atom () {
		switch (tok->type) {

			case T_PAREN_OPEN: {
				// expression in parentheses
				tok++;

				Value result = expression(0);

				if (tok->type != T_PAREN_CLOSE)
					throw_error_after("syntax error, parenthesis '(' not closed", tok[-1]);
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
						args.emplace_back( expression(0) );

						if (tok->type == T_COMMA) {
							tok++;
						}
						else if (tok->type != T_PAREN_CLOSE) {
							throw_error_after("syntax error, ',' or ')' expected!", tok[-1]);
						}
					}
					Token& paren_close = *tok++;

					return call_function(ident.text, args.data(), args.size(), ident, paren_close);
				} else {
					// variable
					auto it = vars.find(ident.text);
					if (it == vars.end())
						throw_error("unknown variable", ident);

					return it->second;
				}
			}

			case T_LITERAL_INT:    return (*tok++).value_int;
			case T_LITERAL_FLOAT:  return (*tok++).value_flt;
			case T_LITERAL_STRING: return parse_escaped_string(*tok++);

			default: {
				throw_error("syntax error, number or variable expected", *tok);
				return {};
			}
		}
	}

	// a series of atoms seperated by binary operators (of precedence higher or equal than min_prec)
	// ex. -x^(y+3) + 5
	// note that the (y+3) is an atom, which happens to be a sub-expression
	// expression calls itself recursively with increasing min_precedences to generate operators in the correct order (precedence climbing algorithm)
	Value expression (unsigned min_prec) {

		Token* unary_minus = nullptr;
		unsigned unary_prec = 1;

		if      (tok->type == T_MINUS) {
			unary_minus = tok++;
			min_prec = std::min(min_prec, unary_prec);
		}
		else if (tok->type == T_PLUS) {
			tok++; // unary plus is no-op
		}

		Value lhs = atom();

		if (unary_minus && is_binary_op(tok->type) && unary_prec >= get_binary_op_associativity(tok->type)) {
			lhs = negate(lhs, *unary_minus);
			unary_minus = nullptr;
		}

		for (;;) {
			TokenType op_type = tok->type;
			if (!is_binary_op(op_type))
				break;

			unsigned prec  = get_binary_op_precedence(   op_type);
			unsigned assoc = get_binary_op_associativity(op_type);

			if (prec < min_prec)
				break;

			Token& op = *tok++; // eat operator

			Value rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			lhs = binop(lhs, rhs, op);
		}

		if (unary_minus)
			lhs = negate(lhs, *unary_minus);
		return lhs;
	}

	void block () {
		if (tok->type != T_BLOCK_OPEN)
			throw_error("syntax error, '{' expected", *tok);
		tok++;

		scope_statements();

		if (tok->type != T_BLOCK_CLOSE)
			throw_error("syntax error, '}' expected", *tok);
		tok++;
	}

	void statement () {
		
		switch (tok[0].type) {

			// block
			case T_BLOCK_OPEN: {
				block();
			} break;

			// for loop
			case T_FOR: {
				
				Token& fortok = *tok++;

				Value count = expression(0);

				if (count.type != INT)
					throw_error_after("syntax error, int expected", fortok);

				Token* loop_start = tok;
				for (int64_t i=0; i<count.i; ++i) {
					tok = loop_start;
					block();
				}
			} break;

			// allow empty statements
			case T_SEMICOLON: {
				// no-op
				tok++;
			} break;

			default: {
				// assignment
				if (tok[0].type == T_IDENTIFIER && tok[1].type == T_EQUALS) {
					Token& variable = *tok++;
					tok++; // T_EQUALS

					Value* lhs = &vars[variable.text];
					*lhs = expression(0);
				}
				// expression (not being assigned)
				else {
					Value result = expression(0);
				}

				if (tok->type != T_SEMICOLON)
					throw_error("syntax error, ';' expected", *tok);
				tok++;
			} break;
		}
	}

	void scope_statements () {
		while (tok->type != T_EOF && tok->type != T_BLOCK_CLOSE) {
			statement();
		}

		//if (tok.peek() != T_EOF) {
		//	throw_error("syntax error, end of input expected", tok.buf[0]);
		//}
	}

	void file () {
		scope_statements();

		if (tok->type != T_EOF)
			throw_error("syntax error, end of input expected", *tok);
	}
};
