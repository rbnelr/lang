#pragma once
#include "common.hpp"
#include "value.hpp"
#include "parser.hpp"

void match_call_args (Value* args, size_t argc, AST_funcdecl const& decl, AST_call* call) {
	AST_var* declarg = (AST_var*)decl.args;

	size_t i = 0;
	while (declarg) {
		if (declarg->a.type == A_VARARGS) {
			// last func arg is varargs, any number of remaining call args works (including 0)
			assert(!declarg->a.next);
			return;
		}

		if (declarg->a.type != A_VARDECL) {
			assert(false);
			return;
		}

		if (i == argc) {
			// no args left in call
			// error: too few arguments
			throw MyException{"error: too few arguments to function", call->a.source};
		}
			
		// still args left in call
		auto& arg = args[i++];

		// TODO: typecheck arg against declarg

		declarg = (AST_var*)declarg->a.next;
	}
	
	// no more args in func
	if (i != argc)
		throw MyException{"error: too many arguments to function", call->a.source};
}

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
			throw MyException{"types do not match", op->a.source};
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
						throw MyException{"can't do math with bool", op->a.source};
					case A_LESS:
					case A_LESSEQ:
					case A_GREATER:
					case A_GREATEREQ:
						throw MyException{"can't compare bools", op->a.source};
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
						throw MyException{"can't do math with str", op->a.source};
				}
			} break;
			case NULL: {
				switch (opt) {
					case A_ADD:
					case A_SUB:
					case A_MUL:
					case A_DIV:
						throw MyException{"can't do math with null", op->a.source};
					case A_LESS:
					case A_LESSEQ:
					case A_GREATER:
					case A_GREATEREQ:
						throw MyException{"null can't be larger or smaller", op->a.source};
				}
			} break;
		}
		assert(false);
		_UNREACHABLE;
	}
	Value unop (Value& rhs, AST_unop* op) {
		switch (rhs.type) {
			case BOOL:
				switch (op->a.type) {
					case A_NOT    : return !rhs.u.b;
				}
				break;
			case INT:
				switch (op->a.type) {
					case A_NEGATE : return -rhs.u.i;
					case A_INC    : return rhs.u.i++;
					case A_DEC    : return rhs.u.i--;
				}
				break;
			case FLT:
				switch (op->a.type) {
					case A_NEGATE : return -rhs.u.f;
					case A_INC    : return rhs.u.f++;
					case A_DEC    : return rhs.u.f--;
				}
				break;
			case NULL: throw MyException{"can't do math with null", op->a.source};
			case STR:  throw MyException{"can't do math with str", op->a.source};
		}
		assert(false);
		_UNREACHABLE;
	}

	std::vector<Value>  stack;

	void stack_push (Value const& val) {
		stack.emplace_back(val);
	}
	Value stack_pop (AST_vardecl* var) {
		auto ret = stack.back();
		stack.pop_back();
		return ret;
	}
	void stack_reset (size_t frame) {
		assert(stack.size() >= frame);
		_ASSUME(stack.size() >= frame);
		stack.resize(frame);
	}

	Value& stack_get (AST_var* var, size_t frame) {
		size_t addr = frame + var->stack_offs;
		assert(addr < stack.size());
		return stack[addr];
	}

	Value call_function (AST_call* call, size_t stack_ptr) {
		auto* func_ast  = call->decl;
		auto& func_decl = ((AST_funcdef*)func_ast)->decl;

		size_t call_frame = stack.size();

		// alloc stack space for returns
		for (size_t i=0; i<func_decl.retc; ++i) {
			stack_push(NULLVAL);
		}
		// alloc stack space for args and fill them
		for (auto* n=call->args; n != nullptr; n = n->next) {
			Value val;
			execute(n, &val, stack_ptr); // read variables while still in old stack frame
			stack_push(val);
		}
		
		// typecheck
		Value* vals = stack.data() + call_frame;
		size_t valc = func_decl.retc + call->argc;
		match_call_args(vals + func_decl.retc, call->argc, func_decl, call);

		// call function with new stack_ptr being the address of the first return
		if (func_ast->type == A_FUNCDEF) {
			auto* funcdef = (AST_funcdef*)func_ast;
			auto* body = (AST_block*)funcdef->body;

			for (auto* n=body->statements; n != nullptr; n = n->next) {
				Value ignore;
				execute(n, &ignore, call_frame);
			}
		}
		else {
			assert(func_ast->type == A_FUNCDEF_BUILTIN);
			auto* decl = (AST_funcdef_builtin*)call->decl;

			decl->func_ptr(vals, valc);
		}

		// get (first) return value from stack
		Value ret = func_decl.retc != 0 ? vals[0] : NULLVAL;

		// reset stack to before the call
		stack_reset(call_frame);

		return ret;
	}

	void execute (AST* node, Value* retval, size_t stack_ptr) {
		//assert(retval->type == NULL);
		//_ASSUME(retval->type == NULL);

		switch (node->type) {

			case A_BLOCK: {
				auto* block = (AST_block*)node;

				// new stack_ptr for scope is the current stack top
				stack_ptr = stack.size();

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					Value ignore;
					execute(n, &ignore, stack_ptr);
				}

				// reset the stack to before the block
				stack.resize(stack_ptr);
			} break;

			case A_LITERAL: {
				auto* lit = (AST_literal*)node;
				*retval = lit->value;
			} break;

			case A_VARDECL: {
				stack_push((AST_vardecl*)node);
			} break;

			case A_VAR: {
				*retval = stack_get((AST_var*)node, stack_ptr);
			} break;

			case A_IF: {
				auto* aif = (AST_if*)node;

				Value condval;
				execute(aif->cond, &condval, stack_ptr);
				if (condval.type != BOOL)
					throw MyException{"if condition must be bool", aif->cond->source};

				auto& body = condval.u.b ? aif->true_body : aif->false_body;
				if (body)
					execute(body, retval, stack_ptr);
			} break;
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				Value condval;
				execute(aif->cond, &condval, stack_ptr);
				if (condval.type != BOOL)
					throw MyException{"select condition must be bool", aif->cond->source};

				auto& body = condval.u.b ? aif->true_body : aif->false_body;
				execute(body, retval, stack_ptr);
			} break;


			case A_FUNCDEF: {
				// does nothing
			} break;

			case A_CALL: {
				*retval = call_function((AST_call*)node, stack_ptr);
			} break;

			case A_LOOP: {
				auto* loop = (AST_loop*)node;
				auto* body = (AST_block*)loop->body;

				// current stack_ptr is the top of the stack
				stack_ptr = stack.size();

				Value startret;
				execute(loop->start, &startret, stack_ptr);

				// stack_ptr to rest loop body variables is the top of the stack after loop <start>
				size_t loop_stack_ptr = stack.size();

				for (;;) {
					Value condval;
					execute(loop->cond, &condval, stack_ptr);
					if (condval.type != BOOL)
						throw MyException{"loop condition must be bool", loop->cond->source};
					if (!condval.u.b)
						break;

					for (auto* n=body->statements; n != nullptr; n = n->next) {
						Value ignore;
						execute(n, &ignore, stack_ptr);
					}

					// pop variables from loop body
					stack.resize(loop_stack_ptr);

					Value endret;
					execute(loop->end, &endret, stack_ptr);
				}

				// pop variables from loop <start>
				stack.resize(stack_ptr);
			} break;

			case A_NEGATE: case A_NOT: {
				auto* op = (AST_unop*)node;

				Value operand_val;
				execute(op->operand, &operand_val, stack_ptr);
				*retval = unop(operand_val, op);
			} break;
			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)node;

				if (op->operand->type != A_VAR)
					throw MyException{"post inc/decrement can only operate on variables", node->source};

				Value& operand_val = stack_get((AST_var*)op->operand, stack_ptr);
				*retval = unop(operand_val, op);
			} break;

			case A_ASSIGN: {
				auto* op = (AST_binop*)node;

				Value tmp;
				execute(op->rhs, &tmp, stack_ptr);

				assert(op->lhs->type == A_VAR || op->lhs->type == A_VARDECL);
				auto* lhs = (AST_var*)op->lhs;

				if (op->lhs->type == A_VARDECL)
					stack_push(tmp);
				else
					stack_get(lhs, stack_ptr) = tmp;
			} break;

			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: {
				auto* op = (AST_binop*)node;

				Value tmp;
				execute(op->rhs, &tmp, stack_ptr);

				assert(op->lhs->type == A_VAR);
				auto* lhs = (AST_var*)op->lhs;

				auto& val = stack_get(lhs, stack_ptr);
				val = binop(val, tmp, assignop2binop(node->type), op); // execute the += as +
			} break;

			case A_ADD: case A_SUB: case A_MUL: case A_DIV:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)node;

				Value l, r;
				execute(op->lhs, &l, stack_ptr);
				execute(op->rhs, &r, stack_ptr);

				*retval = binop(l, r, op->a.type, op);
			} break;

			default:
				assert(false);
				_UNREACHABLE;
		}
	}
};
