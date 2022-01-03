#pragma once
#include "common.hpp"
#include "parser.hpp"
#include "builtins.hpp"

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
			throw CompilerExcept{"error: identifer already declared in this scope", ast->src_tok->source}; // TODO: print declaration of that identifer
	}

	void declare_var (AST_vardecl* var) {
		declare_ident((AST*)var, var->ident);

		vars_stack.emplace_back(var->ident);
	}
	void declare_func (AST_funcdef* func) {
		declare_ident((AST*)func, func->ident);

		funcs_stack.emplace_back(func->ident);
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
		throw CompilerExcept{"error: unknown identifer", node->src_tok->source};
	}
	void resolve_var (AST_var* var) {
		// min_scope = func_scope_id, functions can only access their own variables
		AST* ast = resolve_ident((AST*)var, var->ident, cur_scope.func_scope_id);
		if (ast->type != A_VARDECL)
			throw CompilerExcept{"error: identifer was not declared as variable", var->src_tok->source};

		var->decl = (AST_vardecl*)ast;
	}

	void resolve_func_call (AST_call* call) {
		// min_scope = 0, functions can call all functions visible to them
		AST* ast = resolve_ident((AST*)call, call->ident, 0);
		if (!(ast->type == A_FUNCDEF || ast->type == A_FUNCDEF_BUILTIN))
			throw CompilerExcept{"error: identifer was not declared as function", call->src_tok->source};

		call->fdef = ast;
	}
};

void match_call_args (AST_call* call, AST_funcdecl* fdef) {
	AST_vardecl* declarg = fdef->args;
	AST* callarg = call->args;

	bool default_args = false;

	for (size_t i=0; ; i++) {
		if (!declarg && !callarg) {
			// argument lists match
			return;
		}

		if (!declarg) // no more args in func
			throw CompilerExcept{"error: too many arguments to function", call->src_tok->source};

		if (declarg->type == A_VARARGS) {
			// last func arg is varargs, any number of remaining call args match (including 0)
			assert(!declarg->next);
			return;
		}

		if (declarg->init) {
			default_args = true;
		}

		if (default_args) {
			if (!declarg->init)
				throw CompilerExcept{"error: default arguments can only appear on the end of the argument list", declarg->src_tok->source};

			if (!callarg) {
				// will use default arg
			}
		}
		else {
			if (!callarg) // no args left in call
				throw CompilerExcept{"error: too few arguments to function", call->src_tok->source};
		}

		if (callarg && callarg->valtype != declarg->valtype)
			throw CompilerExcept{"error: call argument type mismatch", callarg->src_tok->source};

		declarg = (AST_vardecl*)declarg->next;
		callarg = callarg ? callarg->next : nullptr;
	}
}

struct IdentResolver {
	IdentiferStack stack;

	std::vector<AST_funcdef*> funcs;

	void resolve (AST* root) {
		for (AST_funcdef_builtin const* f : BUILTIN_FUNCS)
			stack.declare_func((AST_funcdef*)f); // cast is safe

		{ // add a declaration for a main function (global space of the file itself represents the main function)
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF, root->src_tok);
			module_main->ident = "main";
			module_main->retc = 0;
			module_main->rets = nullptr;
			module_main->argc = 0;
			module_main->args = nullptr;
			module_main->body = root;

			funcs.emplace_back(module_main);
		}

		recurse(root);
	}

	void prescan_block_for_funcs (AST_block* block) {
		for (auto* n=block->statements; n != nullptr; n = n->next) {
			if (n->type == A_FUNCDEF) {
				auto* fdef = (AST_funcdef*)n;

				for (auto* n=(AST*)fdef->args; n != nullptr; n = n->next) {
					recurse(n);
				}

				for (auto* n=(AST*)fdef->rets; n != nullptr; n = n->next)
					recurse(n);

				stack.declare_func(fdef);
				funcs.emplace_back(fdef);
			}
		}
	}

	void recurse (AST* node) {
		switch (node->type) {

			case A_LITERAL: {
				auto* lit = (AST_literal*)node;
			} break;

			case A_VARDECL: {
				auto* vardecl = (AST_vardecl*)node;
				stack.declare_var(vardecl);

				if (vardecl->init) {

					recurse(vardecl->init);

					// everything on the rhs of assignments except calls with void return should have a non-void type
					if (vardecl->init->valtype == VOID) {
						assert(vardecl->init->type == A_CALL);
						throw CompilerExcept{"error: variable initialization: void is not a valid variable type", vardecl->init->src_tok->source};
					}

					// variable declaration without explicit type -> infer type
					if (vardecl->valtype == VOID) {
						vardecl->valtype = vardecl->init->valtype;
					}
					// variable declaration with explicit type -> check if types match
					else {
						if (vardecl->valtype != vardecl->init->valtype)
							throw CompilerExcept{"error: variable initialization: types do not match", vardecl->init->src_tok->source};
					}
				}
			} break;

			case A_VAR: {
				auto* var = (AST_var*)node;
				stack.resolve_var(var);
				node->valtype = var->decl->valtype;
			} break;

			case A_ASSIGNOP: {
				auto* op = (AST_binop*)node;
				recurse(op->lhs);
				recurse(op->rhs);

				if (op->lhs->type != A_VAR)
					throw CompilerExcept{"error: can only assign to variables, not arbitrary expressions", op->lhs->src_tok->source};
				
				// everything on the rhs of assignments except calls with void return should have a non-void type
				if (op->rhs->valtype == VOID) {
					assert(op->rhs->type == A_CALL);
					throw CompilerExcept{"error: variable initialization: void is not a valid variable type", op->rhs->src_tok->source};
				}

				// check if types match
				if (op->lhs->valtype != op->rhs->valtype)
					throw CompilerExcept{"error: assignment: types do not match", op->src_tok->source};

				op->valtype = op->lhs->valtype;
			} break;

			case A_CALL: {
				auto* call = (AST_call*)node;
				auto* fdef = (AST_funcdef*)call->fdef;

				stack.resolve_func_call(call);

				for (auto* n=call->args; n != nullptr; n = n->next) {
					recurse(n);
				}

				auto* funcdef = (AST_funcdef*)call->fdef;
				match_call_args(call, funcdef);

				call->valtype = funcdef->retc > 0 ? funcdef->rets->valtype : VOID;
			} break;

			case A_UNOP: {
				auto* op = (AST_unop*)node;
				recurse(op->operand);
				op->valtype = op->operand->valtype;
			} break;

			case A_BINOP: {
				auto* op = (AST_binop*)node;
				recurse(op->lhs);
				recurse(op->rhs);

				if (op->lhs->valtype != op->rhs->valtype)
					throw CompilerExcept{"error: binary operator: types do not match", op->src_tok->source};

				op->valtype = op->lhs->valtype;
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				recurse(aif->cond);
				recurse(aif->if_body);
				if (aif->else_body)
					recurse(aif->else_body);

				if (node->type == A_SELECT) {
					if (aif->if_body->valtype != aif->else_body->valtype)
						throw CompilerExcept{"error: select expression: types do not match", aif->src_tok->source};
					aif->valtype = aif->if_body->valtype;
				}
			} break;

			case A_BLOCK: {
				auto* block = (AST_block*)node;

				auto old_scope = stack.push_scope();

				prescan_block_for_funcs(block); // can call functions before they are declared from anywhere inside the block

				for (auto* n=block->statements; n != nullptr; n = n->next)
					recurse(n);

				stack.reset_scope(old_scope);
			} break;

			case A_FUNCDEF: {
				auto* fdef = (AST_funcdef*)node;

				// returns and args already resolved by prescan

				recurse(fdef->body);
			} break;

			case A_WHILE: {
				auto* loop = (AST_loop*)node;

				assert(!loop->start);
				recurse(loop->cond);
				recurse(loop->body);
				assert(!loop->end);
			} break;

			case A_FOR: {
				auto* loop = (AST_loop*)node;

				// open extra scope for for-header variables
				auto old_scope = stack.push_scope();

				// resolve for-header first so that even though loop->end is executed last
				// it still can't see vars inside the loop (since C does it this way and also since it comes before the body)
				if (loop->start)
					recurse(loop->start);

				recurse(loop->cond);

				if (loop->end)
					recurse(loop->end);

				// resolve body last
				recurse(loop->body);

				stack.reset_scope(old_scope);
			} break;

			// need special handling for do-while due to special scoping rules
			case A_DO_WHILE: {
				auto* loop = (AST_loop*)node;
				auto* block = (AST_block*)loop->body;

				// open scope
				auto old_scope = stack.push_scope();

				// can call functions before they are declared from anywhere inside the block
				// _and_ from inside the while condition
				prescan_block_for_funcs(block);

				for (auto* n=block->statements; n != nullptr; n = n->next)
					recurse(n);
				recurse(loop->cond); // can access vars and funcs declared inside the loop body

				stack.reset_scope(old_scope);
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
};
