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

	size_t scope_depth = 0;

	struct Scope {
		// index of first variable of each scope in <vars_stack>
		size_t vars_base;
		// index of first variable of each scope in <funcs_stack>
		size_t funcs_base;
	};
	Scope cur_scope = {0,0};

	Scope push_scope () {
		Scope old_scope = cur_scope;

		cur_scope.vars_base  = vars_stack .size();
		cur_scope.funcs_base = funcs_stack.size();
		scope_depth++;

		return old_scope;
	}
	void reset_scope (Scope& old_scope) {
		assert(scope_depth > 0);

		scope_depth--;

		for (size_t i=cur_scope.vars_base; i<vars_stack.size(); ++i) {
			ident_map.erase({ scope_depth, vars_stack[i] });
		}
		for (size_t i=cur_scope.funcs_base; i<funcs_stack.size(); ++i) {
			ident_map.erase({ scope_depth, funcs_stack[i] });
		}

		vars_stack .resize(cur_scope.vars_base);
		funcs_stack.resize(cur_scope.funcs_base);

		cur_scope = old_scope;
	}

	void declare_ident (AST* ast, strview const& ident) {
		auto res = ident_map.try_emplace(ScopedIdentifer{ scope_depth-1, ident }, ast);
		if (!res.second)
			throw MyException{"error: identifer already declared in this scope", ast->source}; // TODO: print declaration of that identifer
	}

	void declare_var (AST_vardecl* var) {
		declare_ident((AST*)var, var->ident);

		var->resolve_addr = vars_stack.size();
		vars_stack.emplace_back(var->ident);
	}
	void declare_func (AST_funcdef* func) {
		declare_ident((AST*)func, func->decl.ident);

		funcs_stack.emplace_back(func->decl.ident);
	}

	AST* resolve_ident (AST* node, strview const& ident) {
		assert(scope_depth > 0);

		for (size_t scope_i=scope_depth; scope_i!=0;) {
			--scope_i;

			auto it = ident_map.find({ scope_i, ident });
			if (it != ident_map.end())
				return it->second;
		}

		throw MyException{"error: unknown identifer", node->source};
	}
	void resolve_var (AST_var* var) {
		AST* ast = resolve_ident((AST*)var, var->ident);
		if (ast->type != A_VARDECL)
			throw MyException{"error: identifer was not declared as variable", var->a.source};

		var->decl = ast;

		AST_vardecl* vardecl = (AST_vardecl*)ast;
		var->stack_offs = (intptr_t)vardecl->resolve_addr - (intptr_t)cur_scope.vars_base;
	}

	void resolve_func_call (AST_call* call) {
		AST* ast = resolve_ident((AST*)call, call->ident);
		if (!(ast->type == A_FUNCDEF || ast->type == A_FUNCDEF_BUILTIN))
			throw MyException{"error: identifer was not declared as function", call->a.source};
		
		//AST_funcdef* funcdef = (AST_funcdef*)ast;
		call->decl = ast;
	}
};

struct IdentifierResolve {
	IdentiferStack stack;

	void resolve_idents (AST* root) {
		auto scope = stack.push_scope();

		for (AST_funcdef_builtin const* f : BUILTIN_FUNCS)
			stack.declare_func((AST_funcdef*)f); // cast is safe

		_resolve(root);

		stack.reset_scope(scope);
	}

	void _resolve (AST* node) {
		switch (node->type) {
			case A_BLOCK: {
				auto* block = (AST_block*)node;

				auto old_scope = stack.push_scope();

				for (auto* n=block->statements; n != nullptr; n = n->next)
					_resolve(n);

				stack.reset_scope(old_scope);
			} break;

			case A_LITERAL: {
				// nothing to resolve
			} break;

			case A_VARDECL: {
				stack.declare_var((AST_vardecl*)node);
			} break;
			case A_VAR: {
				stack.resolve_var((AST_var*)node);
			} break;

			case A_FUNCDEF: {
				auto* def = (AST_funcdef*)node;
				auto* body = (AST_block*)def->body;

				stack.declare_func(def);

				auto scope = stack.push_scope();

				for (auto* ret=def->decl.rets; ret != nullptr; ret = ret->next)
					stack.declare_var((AST_vardecl*)ret);

				for (auto* arg=def->decl.args; arg != nullptr; arg = arg->next)
					stack.declare_var((AST_vardecl*)arg);

				for (auto* n=body->statements; n != nullptr; n = n->next)
					_resolve(n);

				stack.reset_scope(scope);
			} break;
			case A_CALL: {
				auto* call = (AST_call*)node;
				stack.resolve_func_call(call);

				for (auto* n=call->args; n != nullptr; n = n->next)
					_resolve(n);
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;
				_resolve(aif->cond     );
				_resolve(aif->true_body);
				if (aif->false_body)
					_resolve(aif->false_body);
			} break;

			case A_LOOP: {
				auto* loop = (AST_loop*)node;
				auto* body = (AST_block*)loop->body;

				auto old_scope = stack.push_scope();

				// resolve control header of for loop first, so that it can't access variables from within the body
				_resolve(loop->start);
				_resolve(loop->cond );
				_resolve(loop->end  );

				for (auto* n=body->statements; n != nullptr; n = n->next)
					_resolve(n);

				stack.reset_scope(old_scope);
			} break;

			case A_RETURN:
			case A_BREAK:
			case A_CONTINUE: {
				// nothing to resolve
			} break;

			case A_NEGATE: case A_NOT: case A_INC: case A_DEC: {
				auto* unop = (AST_unop*)node;
				_resolve(unop->operand);
			} break;

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ: case A_EQUALS: case A_NOT_EQUALS:
			case A_ASSIGN:
			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: case A_REMAINDEREQ: {
				auto* binop = (AST_binop*)node;
				_resolve(binop->lhs);
				_resolve(binop->rhs);
			} break;

			default:
				assert(false);
				_UNREACHABLE;
		}
	}
};
