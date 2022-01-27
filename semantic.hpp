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
	std::vector<strview>                      ident_stack;

	size_t scope_id = 0;

	struct Scope {
		// index of first ident of each scope in <ident_stack>
		// to be able to remove identifiers when the scope is reset
		size_t ident_base;
		// innermost scope that corresponds to a function
		// to enfore functions only being able to access variables outside their scope
		// TODO: allow to capture these variables
		// (but they can access functions outside their scope)
		size_t func_scope_id;
	};
	Scope cur_scope = {0,0};

	Scope push_scope (bool func_scope=false) {
		Scope old_scope = cur_scope;

		scope_id++;

		cur_scope.ident_base = ident_stack.size();

		if (func_scope)
			cur_scope.func_scope_id = scope_id;

		return old_scope;
	}
	void reset_scope (Scope& old_scope) {
		assert(scope_id > 0);

		for (size_t i=cur_scope.ident_base; i<ident_stack.size(); ++i) {
			ident_map.erase({ scope_id, ident_stack[i] });
		}
		ident_stack.resize(cur_scope.ident_base);

		scope_id--;
		cur_scope = old_scope;
	}

	void declare_ident (AST* ast, strview const& ident) {
		auto res = ident_map.try_emplace(ScopedIdentifer{ scope_id, ident }, ast);
		if (!res.second)
			throw CompilerExcept{"error: identifer already declared in this scope", ast->src_tok->source}; // TODO: print declaration of that identifer
		
		ident_stack.emplace_back(ident);
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

struct SemanticAnalysis {
	IdentiferStack stack;

	std::vector<AST_funcdef*> funcs;

	std::vector<AST_funcdef*> funcs_stack;

	void semantic_analysis (AST* root) {
		for (auto* f : builtin_funcs)
			stack.declare_ident(f, f->ident);

		{ // add a declaration for a main function (global space of the file itself represents the main function)
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF, root->src_tok);
			module_main->ident = "main";
			module_main->rets = {};
			module_main->args = {};
			module_main->body = root;

			funcs.emplace_back(module_main);

			funcs_stack.emplace_back(module_main);
		}

		recurse(root);

		assert(funcs_stack.size() == 1);
		funcs_stack.pop_back();
	}

	Type typecheck_unary_op (AST_unop* op, AST* operand) {

		if (op->op == OP_LOGICAL_NOT) {
			// TODO: !<non-bool> should be transformed (in ast?) to !(bool)<non-bool>
			if (operand->valtype != BOOL)
				throw CompilerExcept{"error: logical not (!x) is not valid for type", op->src_tok->source};

			return BOOL;
		}

		switch (operand->valtype) {
		case INT: {
			switch (op->op) {
				case OP_POSITIVE: // no-op
				case OP_NEGATE: 
				case OP_BIT_NOT:
				case OP_INC:    
				case OP_DEC:    
					return operand->valtype;

				INVALID_DEFAULT;
			}
		}
		case BOOL: {
			switch (op->op) {
				case OP_BIT_NOT:
					return operand->valtype;

				case OP_POSITIVE: throw CompilerExcept{"error: positive operator is not valid for bool", op->src_tok->source};
				case OP_NEGATE:   throw CompilerExcept{"error: negate is not valid for bool",            op->src_tok->source};
				case OP_INC:      throw CompilerExcept{"error: increment is not valid for bool",         op->src_tok->source};
				case OP_DEC:      throw CompilerExcept{"error: decrement is not valid for bool",         op->src_tok->source};

				INVALID_DEFAULT;
			}
		}
		case FLT: {
			switch (op->op) {
				case OP_POSITIVE:
				case OP_NEGATE:  
					return operand->valtype;

				case OP_BIT_NOT: throw CompilerExcept{ "error: bitwise operators not valid for floats", op->src_tok->source };

				// NOTE: Maybe this is a weird decision, but incrementing a float rarely makes sense unlike ints
				// Furthermore unlike for ints there is no inc/dec instruction in the cpu either, so maybe don't define this operator for floats
				case OP_INC: throw CompilerExcept{"error: increment is not valid for type (are you sure you want a float?)", op->src_tok->source};
				case OP_DEC: throw CompilerExcept{"error: decrement is not valid for type (are you sure you want a float?)", op->src_tok->source};

				INVALID_DEFAULT;
			}
		}
		INVALID_DEFAULT;
		}
	}
	Type typecheck_binary_op (AST_binop* op, AST* lhs, AST* rhs) {
		if (op->op == OP_LOGICAL_AND || op->op == OP_LOGICAL_OR) {
			// TODO: <non-bool> with and or or should be each transformed (in ast?) to (bool)<non-bool>
			if (lhs->valtype != BOOL)
				throw CompilerExcept{"error: logical not (!x) is not valid for type", lhs->src_tok->source};
			if (rhs->valtype != BOOL)
				throw CompilerExcept{"error: logical not (!x) is not valid for type", rhs->src_tok->source};

			// these operators short-ciruit behavior which is currently implemented in codegen
			// TODO: could also implement this as a AST-transformation?
			// I was thinking as a A_SELECT (lhs == false ? false : rhs)
			// But this might result in slightly worst IR being generated

			// OP_LOGICAL_AND evaluated like:
			//   l : bool = eval lhs;
			//   if l == false: return false;
			//   r : bool = eval rhs;
			//   return r;
			
			// OP_LOGICAL_OR evaluated like:
			//   l : bool = eval lhs;
			//   if l == true: return true;
			//   r : bool = eval rhs;
			//   return r;

			return BOOL;
		}

		// TOOD: there might be operators for which this is not true
		if (lhs->valtype != rhs->valtype)
			throw CompilerExcept{"error: binary operator: types do not match", op->src_tok->source};

		switch (lhs->valtype) {
		case INT: {
			switch (op->op) {
				case OP_ADD:
				case OP_SUB:
				case OP_MUL:
				case OP_DIV:
				case OP_MOD:
					return lhs->valtype;
				
				case OP_BIT_AND:
				case OP_BIT_OR: 
				case OP_BIT_XOR:
					return lhs->valtype;
				
				case OP_LESS:      
				case OP_LESSEQ:    
				case OP_GREATER:   
				case OP_GREATEREQ: 
				case OP_EQUALS:    
				case OP_NOT_EQUALS:
					return BOOL;

				INVALID_DEFAULT;
			}
		} break;
		case BOOL: {
			switch (op->op) {
				case OP_ADD:
				case OP_SUB:
				case OP_MUL:
				case OP_DIV:
				case OP_MOD:
					throw CompilerExcept{ "error: math ops not valid for this type", op->src_tok->source };
		
				case OP_BIT_AND:
				case OP_BIT_OR: 
				case OP_BIT_XOR:
					return lhs->valtype;
				
				case OP_LESS:
				case OP_LESSEQ:
				case OP_GREATER:
				case OP_GREATEREQ:
					throw CompilerExcept{ "error: can't compare bools like that", op->src_tok->source };
		
				case OP_EQUALS:    
				case OP_NOT_EQUALS:
					return BOOL;

				INVALID_DEFAULT;
			}
		} break;
		case FLT: {
			switch (op->op) {
				case OP_ADD:
				case OP_SUB:
				case OP_MUL:
				case OP_DIV:
					return lhs->valtype;

				case OP_MOD:
					throw CompilerExcept{ "error: remainder operator not valid for floats", op->src_tok->source };

				case OP_BIT_AND:
				case OP_BIT_OR:
				case OP_BIT_XOR:
					throw CompilerExcept{ "error: bitwise operators not valid for floats", op->src_tok->source };
				
				// always ordered comparisons (NaN behavior)
				case OP_LESS:
				case OP_LESSEQ:
				case OP_GREATER:
				case OP_GREATEREQ:
				case OP_EQUALS:
				case OP_NOT_EQUALS:
					return BOOL;

				INVALID_DEFAULT;
			}
		} break;
		default:
			throw CompilerExcept{ "error: math ops not valid for this type", op->src_tok->source };
		}
	}
	
	void prescan_block_for_funcs (AST_block* block) {
		for (auto* n : block->statements) {
			if (n->type == A_FUNCDEF) {
				auto* fdef = (AST_funcdef*)n;

				stack.declare_ident(fdef, fdef->ident);
				funcs.emplace_back(fdef);
			}
		}
	}
	
	void resolve_vardecl (AST_vardecl* vardecl) {

		stack.declare_ident(vardecl, vardecl->ident);
		
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
	}

	void resolve_funcdecl_args (arrview<AST_vardecl*> args) {
		
		bool default_args = false;
		
		for (size_t i=0; i<args.count; ++i) {
			auto& arg = args[i];

			if (arg->type == A_VARARGS) {
				if (i != args.count-1)
					throw CompilerExcept{"error: variadic argument can only appear on the end of the argument list", arg->src_tok->source};
				break;
			}

			if (arg->init) {
				default_args = true;
			}
			else {
				if (default_args)
					throw CompilerExcept{"error: default arguments can only appear after all positional arguments", arg->src_tok->source};
			}

			resolve_vardecl(arg);
		}
	}
	
	void resolve_callarg (AST_callarg* callarg, AST_vardecl* declarg) {
		recurse(callarg->expr);
		callarg->valtype = callarg->expr->valtype;

		if (declarg->type != A_VARARGS && callarg->valtype != declarg->valtype)
			throw CompilerExcept{"error: argument type mismatch", callarg->src_tok->source};

		callarg->decl = declarg;
	}
	void resolve_call_args (AST* op, arrview<AST_callarg*> callargs, arrview<AST_vardecl*> declargs) {
		size_t non_vararg_count = 0;
		if (declargs.count > 0)
			non_vararg_count = declargs[declargs.count-1]->type == A_VARARGS ? declargs.count-1 : declargs.count;

		struct Arg {
			AST_vardecl* decl; // funcdef argdecl this callarg is matched to
			AST*         expr;
		};
		smallvec<Arg, 32> args(non_vararg_count);

		//// collect decl args and start them out not provided

		for (size_t i = 0; i < declargs.count; ++i) {
			if (op->type == A_RETURN && declargs[i]->type == A_VARARGS)
				throw CompilerExcept{"error: variadic argument no allowed for return values", declargs[i]->src_tok->source};

			if (declargs[i]->type == A_VARARGS)
				break;

			args[i].decl = declargs[i];
			args[i].expr = nullptr;
		}

		//// positional args
		size_t i = 0;

		for (; i < callargs.count && callargs[i]->ident.empty(); ++i) {
			if (i >= declargs.count) // no more args in func
				throw CompilerExcept{"error: too many arguments", callargs[i]->src_tok->source};

			if (declargs[i]->type == A_VARARGS) {
				auto* vararg_decl = declargs[i];

				for (; i < callargs.count; ++i) {
					assert(callargs[i]->ident.empty());

					resolve_callarg(callargs[i], vararg_decl);

					args.push().expr = callargs[i]->expr;
				}
				break;
			}

			resolve_callarg(callargs[i], declargs[i]);

			args[i].expr = callargs[i]->expr;
		}

		for (; i < callargs.count && callargs[i]->ident.empty(); ++i) {
			if (i >= declargs.count) // no more args in func
				throw CompilerExcept{"error: too many arguments", callargs[i]->src_tok->source};

			if (declargs[i]->type == A_VARARGS) {
				args.push();
			}

			resolve_callarg(callargs[i], declargs[i]);

			args[i].expr = callargs[i]->expr;
		}
		
		//// named args

		auto find_named_arg = [&] (AST_callarg* callarg) {
			for (size_t i=0; i<args.count; ++i) {
				if (args[i].decl->ident == callarg->ident)
					return i;
			}
			throw CompilerExcept{"error: unknown argument", callarg->src_tok->source};
		};
		
		for (; i < callargs.count; ++i) {
			
			if (callargs[i]->ident.empty())
				throw CompilerExcept{"error: named arguments can only appear after all positional arguments", callargs[i]->src_tok->source};

			size_t argi = find_named_arg(callargs[i]);

			if (args[argi].decl->type == A_VARARGS)
				throw CompilerExcept{"error: variadic arguments cannot be assigned directly", callargs[i]->src_tok->source};

			if (args[argi].expr)
				throw CompilerExcept{"error: argument already set in call", callargs[i]->src_tok->source}; // TODO: specify where
			
			resolve_callarg(callargs[i], args[argi].decl);

			args[argi].expr = callargs[i]->expr;
		}

		if (op->type == A_RETURN) {
			// return args can be set like variables outside of the return statement
			// thus we can't check if they are actually provided
		}
		else {
			assert(op->type == A_CALL);
			auto* call = (AST_call*)op;

			auto* resolved_args = g_allocator.alloc_array<AST*>(args.count);
			call->resolved_args = { resolved_args, args.count };

			// check that all callargs are provided
			for (size_t i=0; i<args.count; ++i) {
				if (args[i].expr == nullptr) {
					if (args[i].decl->init == nullptr)
						throw CompilerExcept{"error: required argument not provided", call->src_tok->source}; // TODO: specify which argument
					
					args[i].expr = args[i].decl->init;
				}

				resolved_args[i] = args[i].expr;
			}
		}
	}

	void condition_expr (AST* ast) {
		recurse(ast);
		
		if (ast->valtype != BOOL)
			throw CompilerExcept{"error: condition expression must be a bool", ast->src_tok->source};
	}

	void recurse (AST* ast) {
		switch (ast->type) {

		case A_LITERAL: {
			auto* lit = (AST_literal*)ast;
		} break;

		case A_VARDECL: {
			resolve_vardecl((AST_vardecl*)ast);
		} break;

		case A_VAR: {
			auto* var = (AST_var*)ast;
			stack.resolve_var(var);
			ast->valtype = var->decl->valtype;
		} break;

		case A_ASSIGNOP: {
			auto* op = (AST_binop*)ast;
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

		case A_UNOP: {
			auto* op = (AST_unop*)ast;

			recurse(op->operand);

			op->valtype = typecheck_unary_op(op, op->operand);
		} break;

		case A_BINOP: {
			auto* op = (AST_binop*)ast;

			recurse(op->lhs);
			recurse(op->rhs);

			op->valtype = typecheck_binary_op(op, op->lhs, op->rhs);
		} break;

		case A_IF:
		case A_SELECT: {
			auto* aif = (AST_if*)ast;

			condition_expr(aif->cond);
			recurse(aif->if_body);
			if (aif->else_body) recurse(aif->else_body);

			if (ast->type == A_SELECT) {
				if (aif->if_body->valtype != aif->else_body->valtype)
					throw CompilerExcept{"error: select expression: types do not match", aif->src_tok->source};
				aif->valtype = aif->if_body->valtype;
			}
		} break;

		case A_WHILE: {
			auto* loop = (AST_loop*)ast;

			assert(!loop->start);
			condition_expr(loop->cond);
			recurse(loop->body); // for scoping: rely on body being a block
			assert(!loop->end);
		} break;

		case A_FOR: {
			auto* loop = (AST_loop*)ast;

			// open extra scope for for-header variables
			auto old_scope = stack.push_scope();

			// resolve for-header first so that even though loop->end is executed last
			// it still can't see vars inside the loop (since C does it this way and also since it comes before the body)
			if (loop->start) recurse(loop->start);
			condition_expr(loop->cond);
			if (loop->end)   recurse(loop->end);

			// resolve body last
			recurse(loop->body);

			stack.reset_scope(old_scope);
		} break;

		case A_DO_WHILE: {
			auto* loop = (AST_loop*)ast;

			// need special handling for do-while due to special scoping rules
			auto* block = (AST_block*)loop->body;

			// open scope
			auto old_scope = stack.push_scope();

			// can call functions before they are declared from anywhere inside the block
			// _and_ from inside the while condition
			prescan_block_for_funcs(block);

			// handle body block without opening a scope for it, so that vars are visible to cond
			for (auto* n : block->statements)
				recurse(n);

			condition_expr(loop->cond); // can access vars and funcs declared inside the loop body

			// close the scope after cond
			stack.reset_scope(old_scope);
		} break;

		case A_BREAK:
		case A_CONTINUE: {
			// nothing to resolve
		} break;

		case A_BLOCK: {
			auto* block = (AST_block*)ast;

			auto old_scope = stack.push_scope();

			prescan_block_for_funcs(block); // can call functions before they are declared from anywhere inside the block

			for (auto* n : block->statements)
				recurse(n);

			stack.reset_scope(old_scope);
		} break;

		case A_FUNCDEF: {
			auto* fdef = (AST_funcdef*)ast;

			funcs_stack.emplace_back(fdef);
			auto func_scope = stack.push_scope(true);

			resolve_funcdecl_args(fdef->args);
			resolve_funcdecl_args(fdef->rets);

			recurse(fdef->body);

			stack.reset_scope(func_scope);
			funcs_stack.pop_back();
		} break;

		case A_CALL: {
			auto* call = (AST_call*)ast;

			stack.resolve_func_call(call);
			auto* fdef = (AST_funcdef*)call->fdef;

			resolve_call_args(call, call->args, fdef->args);

			call->valtype = fdef->rets.count > 0 ? fdef->rets[0]->valtype : VOID;
		} break;

		case A_RETURN: {
			auto* ret = (AST_return*)ast;
			auto* fdef = funcs_stack.back();

			resolve_call_args(ret, ret->args, fdef->rets);
		} break;

		INVALID_DEFAULT;
		}
	}
};
