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
		if (ast->type != A_FUNCDEF)
			throw CompilerExcept{"error: identifer was not declared as function", call->src_tok->source};

		call->fdef = ast;
	}
};

struct IdentResolver {
	IdentiferStack stack;

	std::vector<AST_funcdef*> funcs;

	void resolve_ast (AST* root) {
		for (auto* f : builtin_funcs)
			stack.declare_func(f);

		{ // add a declaration for a main function (global space of the file itself represents the main function)
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF, root->src_tok);
			module_main->ident = "main";
			module_main->retc = 0;
			module_main->rets = nullptr;
			module_main->argc = 0;
			module_main->args = nullptr;
			module_main->body = root;

			funcs.emplace_back(module_main);

			funcs_stack.emplace_back(module_main);
		}

		recurse(root);

		assert(funcs_stack.size() == 1);
		funcs_stack.pop_back();
	}

	void resolve_decl_args (AST_vardecl* declarg) {
		bool default_args = false;

		while (declarg) {
			if (declarg->type == A_VARARGS) {
				// last func arg is varargs, any number of remaining call args match (including 0)
				if (declarg->next != nullptr)
					throw CompilerExcept{"error: variadic argument can only appear on the end of the argument list", declarg->src_tok->source};
				break;
			}

			if (declarg->init) {
				default_args = true;
			}
			else {
				if (default_args)
					throw CompilerExcept{"error: default arguments can only appear after all positional arguments", declarg->src_tok->source};
			}

			recurse(declarg);

			declarg = (AST_vardecl*)declarg->next;
		}
	}
	void resolve_call_args (AST* call, AST_callarg* callarg, AST_vardecl* declarg, size_t declargc, bool returns=false) {
		struct Arg {
			AST_vardecl* decl;
			bool         required;
			bool         provided;
		};
		std::vector<Arg> declargs;
		declargs.resize(declargc);

		auto find_named_arg = [] (std::vector<Arg>& declargs, AST_callarg* callarg, size_t* decli) {
			for (size_t i=0; i<declargs.size(); ++i) {
				if (declargs[i].decl->ident == callarg->ident) {
					if (declargs[i].provided)
						throw CompilerExcept{"error: argument already set in call", callarg->src_tok->source};

					declargs[i].provided = true;

					*decli = i;
					return declargs[i].decl;
				}
			}
			throw CompilerExcept{"error: unknown argument", callarg->src_tok->source};
		};

		// collect decl args and have them be not-set
		size_t decli = 0;
		for (AST_vardecl* arg=declarg; arg; arg = (AST_vardecl*)arg->next) {
			if (returns && arg->type == A_VARARGS)
				throw CompilerExcept{"error: variadic argument no allowed for return values", arg->src_tok->source};

			declargs[decli].decl = arg;
			// variables do not required if they are default args or if they are varargs
			declargs[decli].required = arg->type != A_VARARGS && !arg->init;
			declargs[decli].provided = false;
			decli++;
		}

		auto resolve_callarg = [this] (AST_callarg* callarg, AST_vardecl* declarg) {
			recurse(callarg->expr);
			callarg->valtype = callarg->expr->valtype;

			if (declarg->type != A_VARARGS && callarg->valtype != declarg->valtype)
				throw CompilerExcept{"error: argument type mismatch", callarg->src_tok->source};
		};

		decli = 0;
		// positional args
		for (; callarg && callarg->ident.empty(); callarg = (AST_callarg*)callarg->next) {

			if (!declarg) // no more args in func
				throw CompilerExcept{"error: too many arguments", callarg->src_tok->source};

			callarg->decl = declarg;
			callarg->decli = decli;

			resolve_callarg(callarg, declarg);

			declargs[decli].provided = true;

			if (declarg->type != A_VARARGS) {
				declarg = (AST_vardecl*)declarg->next;
				decli++;
			}
		}

		// named args
		for (; callarg; callarg = (AST_callarg*)callarg->next) {
			
			if (callarg->ident.empty())
				throw CompilerExcept{"error: named arguments can only appear after all positional arguments", callarg->src_tok->source};

			callarg->decl = find_named_arg(declargs, callarg, &callarg->decli);

			if (callarg->decl->type == A_VARARGS)
				throw CompilerExcept{"error: variadic arguments cannot be assigned directly", callarg->src_tok->source};

			resolve_callarg(callarg, callarg->decl);
		}

		if (!returns) {
			// check that all args are set
			for (size_t i=0; i<declargs.size(); ++i) {
				if (declargs[i].required && !declargs[i].provided)
					throw CompilerExcept{"error: too few arguments, required argument not provided", call->src_tok->source};
			}
		}
	}

	void prescan_block_for_funcs (AST_block* block) {
		for (auto* n=block->statements; n != nullptr; n = n->next) {
			if (n->type == A_FUNCDEF) {
				auto* fdef = (AST_funcdef*)n;

				stack.declare_func(fdef);
				funcs.emplace_back(fdef);
			}
		}
	}

	std::vector<AST_funcdef*> funcs_stack;

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
				auto* funcdef = (AST_funcdef*)call->fdef;

				resolve_call_args(call, (AST_callarg*)call->args, (AST_vardecl*)funcdef->args, funcdef->argc);

				call->valtype = funcdef->retc > 0 ? funcdef->rets->valtype : VOID;
			} break;

			case A_RETURN: {
				auto* ret = (AST_return*)node;
				auto* fdef = funcs_stack.back();

				resolve_call_args(ret, (AST_callarg*)ret->args, (AST_vardecl*)fdef->rets, fdef->retc, true);
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

				funcs_stack.emplace_back(fdef);
				auto func_scope = stack.push_scope(true);

				resolve_decl_args((AST_vardecl*)fdef->args);
				resolve_decl_args((AST_vardecl*)fdef->rets);

				recurse(fdef->body);

				stack.reset_scope(func_scope, true);
				funcs_stack.pop_back();
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
