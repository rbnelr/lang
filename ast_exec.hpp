#pragma once
#include "common.hpp"
#include "types.hpp"
#include "parser.hpp"

void println (Value& val) {
	print_val(val);
	printf("\n");
}
void my_printf (Value& format, Value const* args, size_t argc) {
	const char* cur = format.u.str;

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

// TODO: make this more dynamic
// function names should be turned into IDs before tokenization
// then put into a hashmap along with the implementation func ptr and a types list to match arguments against
Value call_function (strview const& ident, Value* args, size_t argc, AST* call) {
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

	if (ident == "print") {
		if (argc != 1) goto mismatch;
		print_val(args[0]);
	}
	else if (ident == "println") {
		if (argc != 1) goto mismatch;
		println(args[0]);
	}
	else if (ident == "printf") {
		if (!match(args, argc, { STR }, true)) goto mismatch;
		my_printf(args[0], args+1, argc-1);
	}
	else if (ident == "timer") {
		if (argc != 0) goto mismatch;
		return (int64_t)get_timestamp();
	}
	else if (ident == "timer_end") {
		if (!match(args, argc, { INT })) goto mismatch;
		auto end = (int64_t)get_timestamp();
		return (double)(end - args[0].u.i) / (double)timestamp_freq;
	}
	else {
		throw MyException{"unknown function", call->source};
	}

	return NULLVAL;

mismatch:
	throw MyException{"no matching function overload", call->source};
}

struct Stack {
	std::vector<Value>  values;
	std::vector<size_t> frames;

	Value& push (AST* var) {
		assert(var->var.addr == values.size());
		return values.emplace_back();
	}
	Value& get (AST* var) {
		assert(!frames.empty());
		//size_t frame_ptr = frames.back();

		assert(var->var.addr < values.size());
		return values[var->var.addr]; 
	}

	void begin_scope () {
		frames.emplace_back( values.size() );
	}
	void end_scope () {
		assert(!frames.empty());

		int frame_idx = (int)frames.size()-1;
		values.resize(frames[frame_idx]);
		frames.pop_back();
	}
};

struct Interpreter {

	Value binop (Value& lhs, Value& rhs, ASTType opt, AST* op) {
		Type lt = lhs.type, rt = rhs.type;

		if ((lt == NULL || rt == NULL) &&
			(opt == A_EQUALS || opt == A_NOT_EQUALS)) {
			// something compared to null
			// if both actually null -> true  (null == null)
			// else                  -> false (null == <something>  or  <something> == null)
			return lt == rt;
		}

		if (lt != rt) {
			throw MyException{"types do not match", op->source};
		}
		switch (lt) {
			case INT: {
				int64_t l = lhs.u.i, r = rhs.u.i;
				switch (opt) {
					case A_ADD:        return l +  r;
					case A_SUB:        return l -  r;
					case A_MUL:        return l *  r;
					case A_DIV:        return l /  r;

					case A_LESS:       return l <  r;
					case A_LESSEQ:     return l <= r;
					case A_GREATER:    return l >  r;
					case A_GREATEREQ:  return l >= r;
					case A_EQUALS:     return l == r;
					case A_NOT_EQUALS: return l != r;
				}
			} break;
			case FLT: {
				double l = lhs.u.f, r = rhs.u.f;
				switch (opt) {
					case A_ADD:        return l +  r;
					case A_SUB:        return l -  r;
					case A_MUL:        return l *  r;
					case A_DIV:        return l /  r;

					case A_LESS:       return l <  r;
					case A_LESSEQ:     return l <= r;
					case A_GREATER:    return l >  r;
					case A_GREATEREQ:  return l >= r;
					case A_EQUALS:     return l == r;
					case A_NOT_EQUALS: return l != r;
				}
			} break;
			case BOOL: {
				bool l = lhs.u.b, r = rhs.u.b;
				switch (opt) {
					case A_EQUALS:     return l == r;
					case A_NOT_EQUALS: return l != r;

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
			} break;
			case STR: {
				switch (opt) {
					case A_EQUALS:
					case A_NOT_EQUALS:
					case A_LESS:
					case A_LESSEQ:
					case A_GREATER:
					case A_GREATEREQ: {
						int res = strcmp(lhs.u.str, rhs.u.str);
						
						switch (opt) {
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
			} break;
			case NULL: {
				switch (opt) {
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
			} break;
		}
		assert(false);
		_UNREACHABLE;
	}
	Value unop (Value& rhs, AST* op) {
		switch (rhs.type) {
			case BOOL:
				switch (op->type) {
					case A_NOT    : return !rhs.u.b;
				}
				break;
			case INT:
				switch (op->type) {
					case A_NEGATE : return -rhs.u.i;
					case A_INC    : return rhs.u.i++;
					case A_DEC    : return rhs.u.i--;
				}
				break;
			case FLT:
				switch (op->type) {
					case A_NEGATE : return -rhs.u.f;
					case A_INC    : return rhs.u.f++;
					case A_DEC    : return rhs.u.f--;
				}
				break;
			case NULL: throw MyException{"can't do math with null", op->source};
			case STR:  throw MyException{"can't do math with str", op->source};
		}
		assert(false);
		_UNREACHABLE;
	}

	Stack stack;

	Value execute (AST* node, int depth=0) {
		switch (node->type) {
			case A_BLOCK: {
				stack.begin_scope();

				for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
					execute(n, depth+1);
				}

				stack.end_scope();
				return NULLVAL;
			}

			case A_LITERAL: {
				assert(!node->child);
				return node->literal.value.copy();
			}
			case A_VAR_DECL: {
				assert(!node->child);
				stack.push(node);
				return NULLVAL;
			}
			case A_VAR: {
				assert(!node->child);
				return stack.get(node).copy();
			}
			
			case A_ASSIGN:
			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: {
				AST* lhs = node->child.get();
				AST* rhs = lhs->next.get();
				assert(!rhs->next);
				assert(lhs->type == A_VAR || (node->type == A_ASSIGN && lhs->type == A_VAR_DECL));

				Value tmp = execute(rhs, depth+1);

				assert(!lhs->child);
				if (lhs->type == A_VAR_DECL)
					stack.push(lhs);

				Value& val = stack.get(lhs);

				if (node->type != A_ASSIGN) {
					tmp.assign( binop(val, tmp, assignop2binop(node->type), node) ); // execute the += as +
				}

				val.assign(std::move(tmp));

				return NULLVAL;
			}

			// binary operators
			case A_ADD: case A_SUB: case A_MUL: case A_DIV:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				AST* lhs = node->child.get();
				AST* rhs = lhs->next.get();
				assert(!rhs->next);

				Value l = execute(lhs, depth+1);
				Value r = execute(rhs, depth+1);

				return binop(l, r, node->type, node);
			}

			// unary operators
			case A_NEGATE: case A_NOT:
			case A_INC: case A_DEC: {
				AST* operand = node->child.get();
				assert(!operand->next);

				if (node->type == A_INC || node->type == A_DEC) {
					if (operand->type != A_VAR)
						throw MyException{"post inc/decrement can only operate on variables", node->source};

					Value& operand_val = stack.get(operand);
					return unop(operand_val, node);
				}
				else {
					Value operand_val = execute(operand, depth+1);
					return unop(operand_val, node);
				}
			}

			case A_CALL: {
				std::vector<Value> args;
				args.resize(node->call.argc);

				size_t i = 0;
				for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
					Value val = execute(n, depth+1);
					args[i++] = std::move(val);

					// compiler is too dumb to understand that the moved-from Value is NULL
					// and generates dtor code for no reason, help it understand
					assert(val.type == NULL);
					_ASSUME(val.type == NULL);
				}

				return call_function(node->call.ident, args.data(), args.size(), node);
			}

			// flow control
			case A_LOOP: {
				AST* begin = node->child.get();
				AST* cond  = begin->next.get();
				AST* end   = cond ->next.get();
				AST* body  = end  ->next.get();
				assert(!body->next);

				// need a scope for the variables declared in <begin> and used in <cond> and <end>
				stack.begin_scope();

				execute(begin, depth+1);

				for (;;) {
					Value condval = execute(cond, depth+1);
					if (condval.type != BOOL)
						throw MyException{"loop condition must be bool", cond->source};
					if (!condval.u.b)
						break;

					execute(body, depth+1);

					execute(end, depth+1);
				}

				stack.end_scope();

				return NULLVAL;
			}
			case A_IF: {
				AST* cond       = node     ->child.get();
				AST* true_body  = cond     ->next .get();
				AST* false_body = true_body->next .get();
				assert(!false_body->next);

				Value condval = execute(cond, depth+1);
				if (condval.type != BOOL)
					throw MyException{"loop condition must be bool", cond->source};
				
				AST* body = condval.u.b ? true_body : false_body;
				if (body)
					execute(body, depth+1);

				return NULLVAL;
			}
		}
		assert(false);
		_UNREACHABLE;
	}
};
