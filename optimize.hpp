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

	size_t declare (AST_var* var) {
		assert(!scopes.empty());

		int scope_idx = (int)scopes.size()-1;
		//size_t scope_frame = scopes[scope_idx];
		//assert(scope_frame <= vars_stack.size());
		size_t addr = vars_stack.size();

		auto res = vars_map.try_emplace({ scope_idx, var->ident }, addr);
		if (!res.second)
			throw MyException{"variable already declared in this scope", var->a.source};

		vars_stack.emplace_back(var->ident);

		return addr;
	}

	size_t get (AST_var* var) {
		assert(!scopes.empty());

		for (int scope_idx=(int)scopes.size()-1; scope_idx>=0; --scope_idx) {
			auto it = vars_map.find({ scope_idx, var->ident });
			if (it != vars_map.end())
				return it->second;
		}
		throw MyException{"unknown variable", var->a.source};
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
			case A_LITERAL: {
				// do nothing
			} break;

			case A_VAR_DECL: {
				auto* var = (AST_var*)node;
				var->addr = var_map.declare(var);
			} break;
			case A_VAR: {
				auto* var = (AST_var*)node;
				var->addr = var_map.get(var);
			} break;

			case A_NEGATE: case A_NOT: case A_INC: case A_DEC: {
				auto* unop = (AST_unop*)node;
				map_vars(unop->operand);
			} break;

			case A_ADD: case A_SUB: case A_MUL: case A_DIV:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ: case A_EQUALS: case A_NOT_EQUALS:
			case A_ASSIGN:
			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: {
				auto* binop = (AST_binop*)node;
				map_vars(binop->lhs);
				map_vars(binop->rhs);
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;
				map_vars(aif->cond     );
				map_vars(aif->true_body);
				if (aif->false_body)
					map_vars(aif->false_body);
			} break;

			case A_LOOP: {
				auto* loop = (AST_loop*)node;

				// need a scope for the variables declared in <begin> and used in <cond> and <end>
				var_map.begin_scope();

				map_vars(loop->start);
				map_vars(loop->cond );
				map_vars(loop->body );
				map_vars(loop->end  );

				var_map.end_scope();
			} break;

			case A_CALL: {
				auto* call = (AST_call*)node;

				for (auto* n=call->args; n != nullptr; n = n->next)
					map_vars(n);

			} break;

			case A_BLOCK: {
				auto* block = (AST_block*)node;

				var_map.begin_scope();

				for (auto* n=block->statements; n != nullptr; n = n->next)
					map_vars(n);

				var_map.end_scope();
			} break;

			default:
				assert(false);
				_UNREACHABLE;
		}
	}
};
