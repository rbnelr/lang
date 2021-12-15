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
Value call_function (strview const& ident, Value* args, size_t argc, AST_call* call) {
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

	Value& push (AST_var* var) {
		assert(var->addr == values.size());
		return values.emplace_back();
	}
	Value& get (AST_var* var) {
		assert(!frames.empty());
		//size_t frame_ptr = frames.back();

		assert(var->addr < values.size());
		return values[var->addr]; 
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

	Value binop (Value& lhs, Value& rhs, ASTType opt, AST_binop* op) {
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
	Value unop (Value& rhs, AST_unop* op) {
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

	void execute (AST_base* node, Value* retval, int depth=0) {
		//assert(retval->type == NULL);
		//_ASSUME(retval->type == NULL);

		switch (node->type) {
			case A_LITERAL: {
				auto* lit = (AST_literal*)node;
				*retval = lit->value.copy();
			} break;

			case A_VAR_DECL: {
				stack.push((AST_var*)node);
			} break;
			case A_VAR: {
				*retval = stack.get((AST_var*)node).copy();
			} break;

			case A_ASSIGN: {
				auto* op = (AST_binop*)node;

				Value tmp;
				execute(op->rhs.get(), &tmp, depth+1);

				assert(op->lhs->type == A_VAR || op->lhs->type == A_VAR_DECL);
				auto* lhs = (AST_var*)op->lhs.get();

				if (op->lhs->type == A_VAR_DECL)
					stack.push(lhs);

				auto& val = stack.get(lhs);
				val.set_null();
				val = std::move(tmp);

			} break;

			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: {
				auto* op = (AST_binop*)node;

				Value tmp;
				execute(op->rhs.get(), &tmp, depth+1);

				assert(op->lhs->type == A_VAR);
				auto* lhs = (AST_var*)op->lhs.get();
				auto& val = stack.get(lhs);

				Value tmp2 = binop(val, tmp, assignop2binop(node->type), op); // execute the += as +

				val.set_null();
				val = std::move(tmp2);
				
			} break;

			// unary operators
			case A_NEGATE: case A_NOT: {
				auto* op = (AST_unop*)node;

				Value operand_val;
				execute(op->operand.get(), &operand_val, depth+1);
				*retval = unop(operand_val, op);
			} break;
			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)node;

				if (op->operand->type != A_VAR)
					throw MyException{"post inc/decrement can only operate on variables", node->source};

				Value& operand_val = stack.get((AST_var*)op->operand.get());
				*retval = unop(operand_val, op);
			} break;

			// binary operators
			case A_ADD: case A_SUB: case A_MUL: case A_DIV:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)node;

				Value l, r;
				execute(op->lhs.get(), &l, depth+1);
				execute(op->rhs.get(), &r, depth+1);

				*retval = binop(l, r, op->type, op);
			} break;

			case A_IF: {
				auto* aif = (AST_if*)node;

				Value condval;
				execute(aif->cond.get(), &condval, depth+1);
				if (condval.type != BOOL)
					throw MyException{"if condition must be bool", aif->cond->source};

				auto& body = condval.u.b ? aif->true_body : aif->false_body;
				if (body)
					execute(body.get(), retval, depth+1);
			} break;
			// ternary operator
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				Value condval;
				execute(aif->cond.get(), &condval, depth+1);
				if (condval.type != BOOL)
					throw MyException{"select condition must be bool", aif->cond->source};

				auto& body = condval.u.b ? aif->true_body : aif->false_body;
				execute(body.get(), retval, depth+1);
			} break;

			// flow control
			case A_LOOP: {
				auto* loop = (AST_loop*)node;

				// need a scope for the variables declared in <begin> and used in <cond> and <end>
				stack.begin_scope();

				Value startret;
				execute(loop->start.get(), &startret, depth+1);

				for (;;) {
					Value condval;
					execute(loop->cond.get(), &condval, depth+1);
					if (condval.type != BOOL)
						throw MyException{"loop condition must be bool", loop->cond->source};
					if (!condval.u.b)
						break;

					Value bodyret;
					execute(loop->body.get(), &bodyret, depth+1);
					Value endret;
					execute(loop->end.get(), &endret, depth+1);
				}

				stack.end_scope();
			} break;

			case A_CALL: {
				auto* call = (AST_call*)node;

				std::vector<Value> args;
				args.resize(call->argc);

				size_t i = 0;
				for (auto* n=call->args.get(); n != nullptr; n = n->next.get()) {
					auto& arg = args[i++];
					execute(n, &arg, depth+1);
				}

				*retval = call_function(call->ident, args.data(), args.size(), call);
			} break;

			case A_BLOCK: {
				auto* block = (AST_block*)node;

				stack.begin_scope();

				for (auto* n=block->statements.get(); n != nullptr; n = n->next.get()) {
					Value ignore;
					execute(n, &ignore, depth+1);
				}

				stack.end_scope();
			} break;

			default:
				assert(false);
				_UNREACHABLE;
		}
		return;
	}
};
