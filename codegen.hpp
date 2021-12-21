#pragma once
#include "common.hpp"
#include "parser.hpp"
#include "builtins.hpp"
#include "code.hpp"

struct ScopedIdentifer {
	size_t     scope;
	strview    ident;
};

_FORCEINLINE bool operator== (ScopedIdentifer const& l, ScopedIdentifer const& r) {
	return l.scope == r.scope && l.ident == r.ident;
}
template<> struct std::hash<ScopedIdentifer> {
	_FORCEINLINE std::size_t operator()(ScopedIdentifer const& i) const noexcept {
		return MurmurHash64A(i.ident.data(), i.ident.size(), (uint64_t)i.scope);
	}
};

struct IdentiferStack {
	// scope_idx, identifer -> vardef or funcdef AST
	std::unordered_map<ScopedIdentifer, AST*> ident_map;

	std::vector<strview> vars_stack;
	std::vector<strview> funcs_stack;

	size_t scope_id = 0;

	struct Scope {
		// index of first variable of each scope in <vars_stack>
		size_t vars_base;
		// index of first variable of each scope in <funcs_stack>
		size_t funcs_base;
		// 
		size_t func_vars_base;
		size_t func_scope_id;
	};
	Scope cur_scope = {0,0,0};

	Scope push_scope (bool func_scope=false) {
		Scope old_scope = cur_scope;

		scope_id++;

		cur_scope.vars_base  = vars_stack .size();
		cur_scope.funcs_base = funcs_stack.size();

		if (func_scope) {
			cur_scope.func_vars_base = cur_scope.vars_base;
			cur_scope.func_scope_id = scope_id;
		}

		return old_scope;
	}
	void reset_scope (Scope& old_scope, bool func_scope=false) {
		assert(scope_id > 0);

		for (size_t i=cur_scope.vars_base; i<vars_stack.size(); ++i) {
			ident_map.erase({ scope_id, vars_stack[i] });
		}
		for (size_t i=cur_scope.funcs_base; i<funcs_stack.size(); ++i) {
			ident_map.erase({ scope_id, funcs_stack[i] });
		}

		vars_stack .resize(cur_scope.vars_base);
		funcs_stack.resize(cur_scope.funcs_base);

		scope_id--;
		cur_scope = old_scope;
	}

	void declare_ident (AST* ast, strview const& ident) {
		auto res = ident_map.try_emplace(ScopedIdentifer{ scope_id, ident }, ast);
		if (!res.second)
			throw CompilerExcept{"error: identifer already declared in this scope", ast->source}; // TODO: print declaration of that identifer
	}

	void declare_var (AST_vardecl* var) {
		declare_ident((AST*)var, var->ident);

		vars_stack.emplace_back(var->ident);
	}
	void declare_func (AST_funcdef* func) {
		declare_ident((AST*)func, func->decl.ident);

		funcs_stack.emplace_back(func->decl.ident);
	}

	AST* resolve_ident (AST* node, strview const& ident, size_t min_scope) {
		assert(scope_id > 0);

		for (size_t i=scope_id; ; i--) {
			auto it = ident_map.find({ i, ident });
			if (it != ident_map.end())
				return it->second;

			if (i == min_scope)
				break;
		}
		throw CompilerExcept{"error: unknown identifer", node->source};
	}
	void resolve_var (AST_var* var) {
		// min_scope = func_scope_id, functions can only access their own variables
		AST* ast = resolve_ident((AST*)var, var->ident, cur_scope.func_scope_id);
		if (ast->type != A_VARDECL)
			throw CompilerExcept{"error: identifer was not declared as variable", var->a.source};

		var->decl = (AST_vardecl*)ast;
	}

	void resolve_func_call (AST_call* call) {
		// min_scope = 0, functions can call all functions visible to them
		AST* ast = resolve_ident((AST*)call, call->ident, 0);
		if (!(ast->type == A_FUNCDEF || ast->type == A_FUNCDEF_BUILTIN))
			throw CompilerExcept{"error: identifer was not declared as function", call->a.source};
		
		call->fdef = ast;
	}
};

void match_call_args (AST_call* call, AST_funcdecl const& fdef) {
	AST* declarg = fdef.args;
	AST* callarg = call->args;

	for (size_t i=0; ; i++) {
		if (!declarg && !callarg) {
			// argument lists match
			return;
		}

		if (!declarg) // no more args in func
			throw CompilerExcept{"error: too many arguments to function", call->a.source};

		if (declarg->type == A_VARARGS) {
			// last func arg is varargs, any number of remaining call args match (including 0)
			assert(!declarg->next);
			return;
		}

		assert(declarg->type == A_VARDECL);

		if (!callarg) // no args left in call
			throw CompilerExcept{"error: too few arguments to function", call->a.source};

		if (callarg->valtype != declarg->valtype)
			throw CompilerExcept{"error: call argument type mismatch", callarg->source};

		declarg = declarg->next;
		callarg = callarg->next;
	}
}

struct Codegen {
	IdentiferStack stack;

	std::vector<AST_funcdef*> funcs;

	void generate (AST* root) {
		for (AST_funcdef_builtin const* f : BUILTIN_FUNCS)
			stack.declare_func((AST_funcdef*)f); // cast is safe

		{
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF);
			module_main->a.source = root->source;
			module_main->decl.ident = "main";
			module_main->decl.retc = 0;
			module_main->decl.rets = nullptr;
			module_main->decl.argc = 0;
			module_main->decl.args = nullptr;
			module_main->body = root;

			funcs.emplace_back(module_main);
		}

		{
			ZoneScopedN("resolve");
			resolve(root);
		}

		code.reserve(1024 * 16);

		auto& func = funcs[0];
		//for (auto& func : funcs) {
			ZoneScopedN("codegen_func");
			codegen_funcdef(func);
		//}
	}

	void resolve (AST* node) {
		switch (node->type) {

			case A_LITERAL: {
				auto* lit = (AST_literal*)node;
			} break;

			case A_VARDECL: {
				auto* vardecl = (AST_vardecl*)node;
				stack.declare_var(vardecl);
			} break;

			case A_VAR: {
				auto* var = (AST_var*)node;
				stack.resolve_var(var);
				node->valtype = var->decl->a.valtype;
			} break;

			case A_ASSIGN:
			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: case A_REMAINDEREQ: {
				auto* op = (AST_binop*)node;
				resolve(op->lhs);
				resolve(op->rhs);

				if (op->lhs->type == A_VARDECL) {
					assert(node->type == A_ASSIGN);

					if (op->lhs->valtype == VOID) {
						if (op->rhs->valtype == VOID) {
							assert(op->rhs->type == A_CALL); // everything on the rhs of assignments except calls should have a resolved type
							throw CompilerExcept{"error: assignment: can't assign void return type", op->a.source};
						}
						op->lhs->valtype = op->rhs->valtype;
					}
					else {
						if (op->lhs->valtype != op->rhs->valtype)
							throw CompilerExcept{"error: variable declaration assignment: types do not match", op->a.source};
					}
				}
				else {
					if (op->lhs->valtype != op->rhs->valtype)
						throw CompilerExcept{"error: assignment: types do not match", op->a.source};
				}
			} break;

			case A_CALL: {
				auto* call = (AST_call*)node;
				stack.resolve_func_call(call);

				for (auto* n=call->args; n != nullptr; n = n->next)
					resolve(n);

				auto* funcdef = (AST_funcdef*)call->fdef;
				match_call_args(call, funcdef->decl);
				call->a.valtype = funcdef->decl.retc > 0 ? funcdef->decl.rets->valtype : VOID;
			} break;

			case A_NEGATE: case A_NOT:
			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)node;
				resolve(op->operand);
				op->a.valtype = op->operand->valtype;
			} break;

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)node;
				resolve(op->lhs);
				resolve(op->rhs);

				if (op->lhs->valtype != op->rhs->valtype)
					throw CompilerExcept{"error: binary operator: types do not match", op->a.source};
				
				op->a.valtype = op->lhs->valtype;
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				visit(node, [this] (AST* node) { resolve(node); });

				if (node->type == A_SELECT) {
					if (aif->true_body->valtype != aif->false_body->valtype)
						throw CompilerExcept{"error: select expression: types do not match", aif->a.source};
					aif->a.valtype = aif->true_body->valtype;
				}
			} break;

			case A_BLOCK:
			case A_LOOP:
			case A_FUNCDEF: {
				bool is_func_scope = node->type == A_FUNCDEF;

				auto old_scope = stack.push_scope(is_func_scope);

				visit(node, [this] (AST* n) {
					if (n->type == A_FUNCDEF) {
						auto* def = (AST_funcdef*)n;
						stack.declare_func(def);

						funcs.emplace_back(def);
					}
				});
				visit(node, [this] (AST* node) { resolve(node); });

				stack.reset_scope(old_scope, is_func_scope);
			} break;

			case A_RETURN:
			case A_BREAK:
			case A_CONTINUE: {
				// nothing to resolve
			} break;

			default: {
				assert(false);
				_UNREACHABLE;
			}
		}
	}

	void codegen_funcdef (AST_funcdef* func) {

		for (auto* n=func->decl.args; n != nullptr; n = n->next) {
			
		}
		for (auto* n=func->decl.rets; n != nullptr; n = n->next) {
			
		}

		size_t stack_frame = cstack.stack.size();

		for (auto* n=func->body; n != nullptr; n = n->next)
			codegen(n);

		pop_cstack(stack_frame);

		code.push_back({ OP_RET, {}, {} });
	}

	std::vector<Instruction> code;

	struct CodegenStack {
		std::vector<AST_vardecl*> stack;
	};
	CodegenStack cstack;

	void pop_cstack (size_t frame) {
		assert(cstack.stack.size() >= frame);
		for (size_t i=cstack.stack.size(); i>frame;) {
			--i;

			code.push_back({ OP_POP, { 0, (AST*)cstack.stack[i] }, { 0, nullptr } });
		}
		cstack.stack.resize(frame);
	}

	static inline AST_vardecl TMP  = { { A_VARDECL, "" }, "tmp", 0 };
	static inline AST_vardecl TMPL = { { A_VARDECL, "" }, "tmpL", 0 };
	static inline AST_vardecl TMPR = { { A_VARDECL, "" }, "tmpR", 0 };

	typedef AST* Jump_t;
	static inline constexpr AST* JUMP_NORMAL = nullptr;

	Jump_t codegen (AST* node, AST_vardecl* dst=nullptr, size_t dst_stk_loc=0) {
		switch (node->type) {

			case A_LITERAL: {
				auto* lit = (AST_literal*)node;
				if (dst) {
					int64_t val = *(int64_t*)&lit->value;
					code.push_back({ OP_MOVI, { (int64_t)dst_stk_loc, (AST*)dst }, { val, node } });
				}
			} break;

			case A_VARDECL: {
				auto* vardecl = (AST_vardecl*)node;

				vardecl->stack_loc = cstack.stack.size();
				cstack.stack.emplace_back(vardecl);

				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)vardecl } });
			} break;
			case A_VAR: {
				auto* var = (AST_var*)node;
				auto* vardecl = (AST_vardecl*)var->decl;

				if (dst) {
					code.push_back({ OP_MOV, { (int64_t)dst_stk_loc, (AST*)dst }, { vardecl->stack_loc, node } });
				}
			} break;

			case A_ASSIGN: {
				auto* op = (AST_binop*)node;

				codegen(op->lhs);
				auto* vardecl = op->lhs->type == A_VARDECL ? (AST_vardecl*)node : (AST_vardecl*)((AST_var*)node)->decl;

				codegen(op->rhs, vardecl, vardecl->stack_loc);
			} break;

			case A_BLOCK: {
				auto* block = (AST_block*)node;

				size_t stack_frame = cstack.stack.size();

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					codegen(n);
				}

				pop_cstack(stack_frame);
			} break;

			case A_CALL: {
				auto* call = (AST_call*)node;

				size_t stack_frame = cstack.stack.size();

				auto* fdef = (AST_funcdef*)call->fdef;
				auto* farg = fdef->decl.args;

				for (auto* n=fdef->decl.rets; n != nullptr; n = n->next) {
					size_t stack_loc = cstack.stack.size();
					cstack.stack.emplace_back((AST_vardecl*)n);

					code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)n } });
				}
				for (auto* n=call->args; n != nullptr; n = n->next) {
					
					auto* argdecl = (AST_vardecl*)farg;
					size_t stack_loc = cstack.stack.size();// don't clobber AST_funcdef.decl.args
					cstack.stack.emplace_back(argdecl);

					code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)argdecl } });

					//
					codegen(n, argdecl, stack_loc);

					if (farg->type != A_VARARGS) farg = farg->next;
				}

				if (call->fdef->type == A_FUNCDEF_BUILTIN) {
					auto* fdefb = (AST_funcdef_builtin*)fdef;
					code.push_back({ OP_CALLB, { (int64_t)fdefb->func_ptr, (AST*)fdefb }, { (int64_t)stack_frame, nullptr } });
				}
				else {
					//code.push_back({ OP_CALL, { 0, call->fdef }, { 0, nullptr } });
				}

				if (dst && fdef->decl.retc > 0) {
					code.push_back({ OP_MOV, { (int64_t)dst_stk_loc, (AST*)dst }, { (int64_t)stack_frame, (AST*)fdef->decl.rets } });
				}

				pop_cstack(stack_frame);
			} break;

			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: case A_REMAINDEREQ: {
				auto* op = (AST_binop*)node;
				assert(op->lhs->type == A_VAR);
				auto* lhs = (AST_var*)op->lhs;

				size_t tmp_loc = cstack.stack.size();
				cstack.stack.emplace_back(&TMP);
				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMP } });

				codegen(op->rhs, &TMP, tmp_loc);

				if (op->lhs->valtype != op->rhs->valtype)
					throw CompilerExcept{"error: compund assignment operator: types do not match", op->a.source};

				Opcode opc;
				switch (op->lhs->valtype) {
					case INT: opc = OP_ADD; break;
					case FLT: opc = OP_FADD; break;
					default:
						throw CompilerExcept{"error: math ops not valid for this type", node->source};
				}
				
				if (op->lhs->valtype == FLT && node->type == A_REMAINDEREQ)
					throw CompilerExcept{"error: remainder operator not valid for floats", node->source};

				opc = (Opcode)(node->type + (opc - A_ADDEQ));

				code.push_back({ opc, { (int64_t)lhs->decl->stack_loc, (AST*)lhs->decl }, { (int64_t)tmp_loc, (AST*)&TMP } });

				pop_cstack(tmp_loc);
			} break;

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)node;

				size_t tmp_locs = cstack.stack.size();

				cstack.stack.emplace_back(&TMPL);
				cstack.stack.emplace_back(&TMPR);
				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMPL } });
				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMPR } });

				codegen(op->lhs, &TMPL, tmp_locs);
				codegen(op->rhs, &TMPR, tmp_locs+1);

				if (op->lhs->valtype != op->rhs->valtype)
					throw CompilerExcept{"error: binary operator: types do not match", op->a.source};

				Opcode opc;
				bool flip = false;

				switch (op->lhs->valtype) {
					case INT: {
						switch (node->type) {
							case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
								opc = (Opcode)(node->type + (OP_ADD - A_ADD));
								break;
							case A_LESS:       opc = OP_LT;                 break;
							case A_LESSEQ:     opc = OP_LTE;                break;
							case A_GREATER:    opc = OP_LTE;  flip = true;  break;
							case A_GREATEREQ:  opc = OP_LT;   flip = true;  break;
							case A_EQUALS:     opc = OP_EQ;                 break;
							case A_NOT_EQUALS: opc = OP_NEQ;                break;
							default: assert(false);
						}
					} break;
					case BOOL: {
						switch (node->type) {
							case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
								throw CompilerExcept{"error: math ops not valid for this type", node->source};
							case A_LESS:     
							case A_LESSEQ:   
							case A_GREATER:  
							case A_GREATEREQ:
								throw CompilerExcept{"error: can't compare bools like that", node->source};

							case A_EQUALS:     opc = OP_EQ;                 break;
							case A_NOT_EQUALS: opc = OP_NEQ;                break;
							default: assert(false);
						}
					} break;
					case FLT: {
						switch (node->type) {
							case A_ADD: case A_SUB: case A_MUL: case A_DIV:
								opc = (Opcode)(node->type + (OP_FADD - A_ADD));
								break;
							case A_REMAINDER:
								throw CompilerExcept{"error: remainder operator not valid for floats", node->source};
							case A_LESS:       opc = OP_FLT;                 break;
							case A_LESSEQ:     opc = OP_FLTE;                break;
							case A_GREATER:    opc = OP_FLT;   flip = true;  break;
							case A_GREATEREQ:  opc = OP_FLTE;  flip = true;  break;
							case A_EQUALS:     opc = OP_FEQ;                 break;
							case A_NOT_EQUALS: opc = OP_FNEQ;                break;
							default: assert(false);
						}
					} break;
					default:
						throw CompilerExcept{"error: math ops not valid for this type", node->source};
				}

				size_t bdst = flip ? tmp_locs+1 : tmp_locs  ;
				size_t bsrc = flip ? tmp_locs   : tmp_locs+1;

				AST* bdstp = flip ? (AST*)&TMPR : (AST*)&TMPL;
				AST* bsrcp = flip ? (AST*)&TMPL : (AST*)&TMPR;

				code.push_back({ opc, { (int64_t)bdst, bdstp }, { (int64_t)bsrc, bsrcp } });

				if (dst) {
					code.push_back({ OP_MOV, { (int64_t)dst_stk_loc, (AST*)dst }, { (int64_t)bdst, bdstp } });
				}
				pop_cstack(tmp_locs);
			} break;

			case A_NEGATE: case A_NOT: {
				auto* op = (AST_unop*)node;

				size_t tmp_loc = cstack.stack.size();
				cstack.stack.emplace_back(&TMP);
				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMP } });

				codegen(op->operand, &TMP, tmp_loc);

				Opcode opc;
				switch (node->type) {
					case A_NEGATE: {
						switch (op->operand->valtype) {
							case INT: opc = OP_NEG; break;
							case FLT: opc = OP_FNEG; break;
							default: throw CompilerExcept{"error: negate is not valid for type", node->source};
						}
					} break;
					case A_NOT: {
						switch (op->operand->valtype) {
							case INT : opc = OP_NOT; break;
							case BOOL: opc = OP_NOT; break;
							default: throw CompilerExcept{"error: not is not valid for type", node->source};
						}
					} break;
				}

				code.push_back({ opc, { (int64_t)tmp_loc, (AST*)&TMP }, { 0, nullptr } });

				if (dst) {
					code.push_back({ OP_MOV, { (int64_t)dst_stk_loc, (AST*)dst }, { (int64_t)tmp_loc, (AST*)&TMP } });
				}
				pop_cstack(tmp_loc);
			} break;

			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)node;
				assert(op->operand->type == A_VAR);
				auto* operand = (AST_var*)op->operand;

				size_t tmp_loc = cstack.stack.size();
				if (dst) {
					cstack.stack.emplace_back(&TMP);

					code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMP } });
					code.push_back({ OP_MOV, { (int64_t)tmp_loc, (AST*)&TMP }, { operand->decl->stack_loc, op->operand } });
				}

				code.push_back({ (Opcode)(node->type + (OP_INC - A_INC)), { operand->decl->stack_loc, op->operand }, { 0, nullptr } });

				if (dst) {
					code.push_back({ OP_MOV, { (int64_t)dst_stk_loc, (AST*)dst }, { (int64_t)tmp_loc, (AST*)&TMP } });
					pop_cstack(tmp_loc);
				}
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				size_t tmp_loc = cstack.stack.size();
				cstack.stack.emplace_back(&TMP);
				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMP } });

				// condition
				codegen(aif->cond, &TMP, tmp_loc);

				size_t false_jmp = code.size();
				code.push_back({ OP_JZ, { 0, nullptr }, { (int64_t)tmp_loc, (AST*)&TMP } });

				// true body
				codegen(aif->true_body, dst, dst_stk_loc);

				if (!aif->false_body) {
					code[false_jmp].dst = { (int64_t)code.size(), nullptr };
				}
				else {
					size_t end_jmp = code.size();
					code.push_back({ OP_JMP, { 0, nullptr }, { 0, nullptr } });

					code[false_jmp].dst = { (int64_t)code.size(), nullptr };

					// false body
					codegen(aif->false_body, dst, dst_stk_loc);

					code[end_jmp].dst = { (int64_t)code.size(), nullptr };
				}

				pop_cstack(tmp_loc);

			} break;

			case A_LOOP: {
				auto* loop = (AST_loop*)node;

				size_t loop_stk = cstack.stack.size();

				// start
				codegen(loop->start);

				size_t loop_lbl = code.size();

				size_t cond_loc = cstack.stack.size();
				cstack.stack.emplace_back(&TMP);
				code.push_back({ OP_PUSHI, { 0, nullptr }, { 0, (AST*)&TMP } });
				
				// condition
				codegen(loop->cond, &TMP, cond_loc);

				// jump to end on cond == false
				size_t cond_jmp = code.size();
				code.push_back({ OP_JZ, { 0, nullptr }, { (int64_t)cond_loc, (AST*)&TMP } });
				
				// body
				codegen(loop->body);

				pop_cstack(cond_loc);

				// end
				codegen(loop->end);

				// unconditional jump to loop top
				code.push_back({ OP_JMP, { (int64_t)loop_lbl, nullptr }, { 0, nullptr } });

				code[cond_jmp].dst = { (int64_t)code.size(), nullptr };

				pop_cstack(loop_stk);
			} break;

			case A_RETURN:
			case A_BREAK:
			case A_CONTINUE: {
				return node;
			} break;

			case A_FUNCDEF:
			default: {
				
			} break;
		}

		return JUMP_NORMAL;
	}
};
