#pragma once
#include "common.hpp"
#include "types.hpp"
#include "parser.hpp"

#ifdef __GNUC__ // GCC 4.8+, Clang, Intel and other compilers compatible with GCC (-std=c++0x or above)
	#define _UNREACHABLE __builtin_unreachable()
#elif defined(_MSC_VER) // MSVC
	#define _UNREACHABLE __assume(false)
#else
	#define _UNREACHABLE 
#endif

void println (Value& val) {
	print_val(val);
	printf("\n");
}

void my_printf (Value& format, Value const* args, size_t argc) {
	const char* cur = format.str;

	size_t i = 0;
	while (*cur != '\0') {
		if (*cur == '{') {
			const char* params = cur++;
			
			while (*cur != '}')
				cur++;
			cur++;

			if (i >= argc) print_val(Value{}); // print null for % that access outside of the varargs
			else           print_val(args[i++]);
			continue;
		} else {
			if (cur[0] == '^' && cur[1] == '{') {
				cur++; // ^{ escape sequence
			}
			putc(*cur++, stdout);
		}
	}
	// ignore varargs that are not printed (no error)
}

Value call_function (strview const& name, Value* args, size_t argc, AST* call) {
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
	else if (name == "timer") {
		if (argc != 0) goto mismatch;
		return (int64_t)get_timestamp();
	}
	else if (name == "timer_end") {
		if (!match(args, argc, { INT })) goto mismatch;
		auto end = (int64_t)get_timestamp();
		return (float)(end - args[0].i) / (float)timestamp_freq;
	}
	else {
		throw MyException{"unknown function", call->source};
	}

	return {};

mismatch:
	throw MyException{"no matching function overload", call->source};
}

struct Interpreter {

	Value binop (Value& lhs, Value& rhs, AST* op) {
		if ((lhs.type == NULL || rhs.type == NULL) &&
			(op->type == A_EQUALS || op->type == A_NOT_EQUALS)) {
			// something compared to null
			// if both actually null -> true  (null == null)
			// else                  -> false (null == <something>  or  <something> == null)
			return lhs.type == rhs.type;
		}

		if (lhs.type != rhs.type) {
			throw MyException{"types do not match", op->source};
		}
		switch (lhs.type) {
			case INT:
				switch (op->type) {
					case A_ADD:        return lhs.i + rhs.i;
					case A_SUB:        return lhs.i - rhs.i;
					case A_MUL:        return lhs.i * rhs.i;
					case A_DIV:        return lhs.i / rhs.i;

					case A_LESS:       return lhs.i <  rhs.i;
					case A_LESSEQ:     return lhs.i <= rhs.i;
					case A_GREATER:    return lhs.i >  rhs.i;
					case A_GREATEREQ:  return lhs.i >= rhs.i;
					case A_EQUALS:     return lhs.i == rhs.i;
					case A_NOT_EQUALS: return lhs.i != rhs.i;
				}
			case FLT:
				switch (op->type) {
					case A_ADD:        return lhs.f + rhs.f;
					case A_SUB:        return lhs.f - rhs.f;
					case A_MUL:        return lhs.f * rhs.f;
					case A_DIV:        return lhs.f / rhs.f;

					case A_LESS:       return lhs.f <  rhs.f;
					case A_LESSEQ:     return lhs.f <= rhs.f;
					case A_GREATER:    return lhs.f >  rhs.f;
					case A_GREATEREQ:  return lhs.f >= rhs.f;
					case A_EQUALS:     return lhs.f == rhs.f;
					case A_NOT_EQUALS: return lhs.f != rhs.f;
				}

			case BOOL:
				switch (op->type) {
					case A_EQUALS:     return lhs.b == rhs.b;
					case A_NOT_EQUALS: return lhs.b != rhs.b;

					case A_ADD:
					case A_SUB:
					case A_MUL:
					case A_DIV:
						throw MyException{"can't do math with bool", op->source};
					case A_LESS:
					case A_LESSEQ:
					case A_GREATER:
					case A_GREATEREQ:
						throw MyException{"can't compare bools", op->source};
				}

			case STR:
				switch (op->type) {
					case A_EQUALS:
					case A_NOT_EQUALS:
					case A_LESS:
					case A_LESSEQ:
					case A_GREATER:
					case A_GREATEREQ: {
						int res = strcmp(lhs.str, rhs.str);
						
						switch (op->type) {
							case A_EQUALS:     return res == 0;
							case A_NOT_EQUALS: return res != 0;
							case A_LESS:       return res <  0;
							case A_LESSEQ:     return res <= 0;
							case A_GREATER:    return res >  0;
							case A_GREATEREQ:  return res >= 0;
						}
					}

					case A_ADD:
					case A_SUB:
					case A_MUL:
					case A_DIV:
						throw MyException{"can't do math with str", op->source};
				}

			case NULL:
				switch (op->type) {
					case A_ADD:
					case A_SUB:
					case A_MUL:
					case A_DIV:
						throw MyException{"can't do math with null", op->source};
					case A_LESS:
					case A_LESSEQ:
					case A_GREATER:
					case A_GREATEREQ:
						throw MyException{"null can't be larger or smaller", op->source};
				}
		}
		assert(false);
		_UNREACHABLE;
	}

	Value unop (Value& rhs, AST* op) {
		switch (rhs.type) {
			case BOOL:
				switch (op->type) {
					case A_NOT    : return !rhs.b;
				}
			case INT:
				switch (op->type) {
					case A_NEGATE : return -rhs.i;
				}
			case FLT:
				switch (op->type) {
					case A_NEGATE : return -rhs.f;
				}
			case NULL: throw MyException{"can't do math with null", op->source};
			case STR:  throw MyException{"can't do math with str", op->source};
		}
		assert(false);
		_UNREACHABLE;
	}

	//struct VarID {
	//	int     scope;
	//	strview name;
	//};
	typedef std::unordered_map<strview, Value> Vars;

	Vars vars;

	Value execute (AST* node, int depth=0) {
		Value ret = {};

		switch (node->type) {
			case A_BLOCK: {
				for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
					execute(n, depth+1);
				}
			} break;

			// values
			case A_CONSTANT: {
				assert(!node->child);
				ret = node->value.copy();
			} break;

			case A_VARIABLE: {
				assert(!node->child);
				auto it = vars.find(node->source.text());
				if (it == vars.end())
					throw MyException{"unknown variable", node->source};
				ret = it->second.copy();
			} break;

			case A_ASSIGNMENT: {
				AST* lhs = node->child.get();
				AST* rhs = lhs->next.get();
				assert(!rhs->next);
				
				if (lhs->type != A_VARIABLE)
					throw MyException{"variable expected on left side of assigment", lhs->source};

				assert(!lhs->child);
				Value& val = vars[lhs->source.text()];
				val = execute(rhs, depth+1);
			} break;

			case A_CALL: {
				std::vector<Value> args;
				args.reserve(32);

				for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
					Value arg = execute(n, depth+1);
					args.emplace_back(std::move(arg));
				};

				ret = call_function(node->source.text(), args.data(), args.size(), node);
			} break;

			// flow control
			case A_LOOP: {
				AST* begin = node->child.get();
				AST* cond  = begin->next.get();
				AST* end   = cond ->next.get();
				AST* body  = end  ->next.get();
				assert(!body->next);

				execute(begin, depth+1);

				for (;;) {
					Value condval = execute(cond, depth+1);
					if (condval.type != BOOL)
						throw MyException{"loop condition must be bool", cond->source};
					if (!condval.b)
						break;

					execute(body, depth+1);

					execute(end, depth+1);
				}
			} break;

			// binary operators
			case A_ADD:
			case A_SUB:
			case A_MUL:
			case A_DIV:
			case A_LESS:
			case A_LESSEQ:
			case A_GREATER:
			case A_GREATEREQ:
			case A_EQUALS:
			case A_NOT_EQUALS: {
				AST* lhs = node->child.get();
				AST* rhs = lhs->next.get();
				assert(!rhs->next);

				Value l = execute(lhs, depth+1);
				Value r = execute(rhs, depth+1);
				
				ret = binop(l, r, node);
			} break;

			// unary operators
			case A_NOT:
			case A_NEGATE: {
				AST* operand = node->child.get();
				assert(!operand->next);

				Value operand_val = execute(operand, depth+1);

				ret = unop(operand_val, node);
			} break;

			default:
				assert(false);
		}

		return ret;
	}
};
