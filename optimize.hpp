#pragma once
#include "common.hpp"
#include "parser.hpp"

struct ScopedIdentifer {
	int        scope;
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
struct VariableMapper {
	// scope_idx, name -> var_idx
	std::unordered_map<ScopedIdentifer, size_t> vars_map;
	// list of all variable names in order of declaration (of all scopes)
	std::vector<strview>                        vars_stack;
	// index of first variable of each scope in <vars_stack>
	std::vector<size_t>                         scopes;

	size_t declare (AST* var) {
		assert(!scopes.empty());

		int scope_idx = (int)scopes.size()-1;
		//size_t scope_frame = scopes[scope_idx];
		//assert(scope_frame <= vars_stack.size());
		size_t addr = vars_stack.size();

		auto res = vars_map.try_emplace({ scope_idx, var->var.ident }, addr);
		if (!res.second)
			throw MyException{"variable already declared in this scope", var->source};

		vars_stack.emplace_back(var->var.ident);

		return addr;
	}

	size_t get (AST* var) {
		assert(!scopes.empty());

		for (int scope_idx=(int)scopes.size()-1; scope_idx>=0; --scope_idx) {
			auto it = vars_map.find({ scope_idx, var->var.ident });
			if (it != vars_map.end())
				return it->second;
		}
		throw MyException{"unknown variable", var->source};
	}

	void begin_scope () {
		scopes.emplace_back( vars_stack.size() );
	}
	void end_scope () {
		assert(!scopes.empty());

		int scope_idx = (int)scopes.size()-1;
		size_t frame = scopes[scope_idx];

		for (size_t i=frame; i<vars_stack.size(); ++i) {
			vars_map.erase({ scope_idx, vars_stack[i] });
		}

		vars_stack.resize(frame);
		scopes.pop_back();
	}
};

struct OptimizePasses {
	VariableMapper var_map;

	void map_vars (AST* node) {
		switch (node->type) {
			case A_BLOCK: {
				var_map.begin_scope();

				for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
					map_vars(n);
				}

				var_map.end_scope();
			} break;
			
			case A_VAR_DECL: {
				assert(!node->child);
				node->var.addr = var_map.declare(node);
			} break;
			case A_VAR: {
				assert(!node->child);
				node->var.addr = var_map.get(node);
			} break;

			case A_LOOP: {
				AST* begin = node->child.get();
				AST* cond  = begin->next.get();
				AST* end   = cond ->next.get();
				AST* body  = end  ->next.get();
				assert(!body->next);

				// need a scope for the variables declared in <begin> and used in <cond> and <end>
				var_map.begin_scope();

				map_vars(begin);
				map_vars(cond);
				map_vars(body);
				map_vars(end);

				var_map.end_scope();
			} break;

			default: {
				for (auto* n=node->child.get(); n != nullptr; n = n->next.get()) {
					map_vars(n);
				}
			} break;
		}
	}
};
