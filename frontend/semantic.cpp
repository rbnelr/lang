#include "common.hpp"
#include "semantic.hpp"
#include "types.hpp"
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
	// (scope_id, identifer) -> AST_vardecl* or AST_funcdef* or AST_type*
	std::unordered_map<ScopedIdentifer, AST*> ident_map;
	std::vector<strview>                      ident_stack;

	size_t scope_id = 0;

	struct Scope {
		// index of first ident of each scope in <ident_stack>
		// to be able to remove identifiers when the scope is reset
		size_t ident_base;
		// innermost scope that corresponds to a function
		// to enfore functions only being able to access variables outside their scope
		// TODO: figure out a way for nested functions to capture variables in the future
		size_t func_scope_id;
	};
	Scope cur_scope = { 0,0 };

	// call to open a new scope
	Scope push_scope (bool func_scope=false) {
		Scope old_scope = cur_scope;

		scope_id++;

		cur_scope.ident_base = ident_stack.size();

		if (func_scope)
			cur_scope.func_scope_id = scope_id;

		return old_scope;
	}
	// call to close the previously opened scope and remove all the contained idents
	void reset_scope (Scope& old_scope) {
		assert(scope_id > 0);

		for (size_t i=cur_scope.ident_base; i<ident_stack.size(); ++i) {
			ident_map.erase({ scope_id, ident_stack[i] });
		}
		ident_stack.resize(cur_scope.ident_base);

		scope_id--;
		cur_scope = old_scope;
	}

	// declare a identifer mapping to an AST node in the current scope
	void declare_ident (AST* ast, strview const& ident) {
		auto res = ident_map.try_emplace(ScopedIdentifer{ scope_id, ident }, ast);
		if (!res.second)
			throw CompilerExcept({ "error", ast->src, "identifer already declared in this scope" },
			                    {{ "note", res.first->second->src, "declared here" }});
		
		ident_stack.emplace_back(ident);
	}

	// resolve an identifier by looking for a (scope_id, identifer) key in the ident_map hashmap
	// do this by trying multiple scope ids starting from the current scope and going down until reaching min_scope
	// this enables variable shadowing and min_scope can be used to limit the search for only variables in the current function by using cur_scope.func_scope_id
	AST* resolve_ident (SourceRange const& src, strview ident, size_t min_scope) {
		assert(scope_id > 0);

		for (size_t i=scope_id; ; i--) {
			auto it = ident_map.find({ i, ident });
			if (it != ident_map.end())
				return it->second;

			if (i == min_scope)
				break;
		}
		ERROR(src, "unknown identifer");
	}

	// resolve a variable identifier inside the current function
	void resolve_var (AST_var* var) {
		// min_scope = func_scope_id, functions can only access their own variables
		AST* ast = resolve_ident(var->src, var->ident, cur_scope.func_scope_id);
		if (ast->kind != A_VARDECL)
			throw CompilerExcept({ "error", var->src, "identifer was not declared as a variable" },
			                    {{ "note", ast->src, "declared here" }});

		var->decl = (AST_vardecl*)ast;
	}
	
	// resolve a function identifier from any relevant scope
	void resolve_func_call (AST_call* call) {
		// min_scope = 0, functions can call all functions visible to them
		AST* ast = resolve_ident(call->src, call->ident, 0);
		if (ast->kind != A_FUNCDEF)
			throw CompilerExcept({ "error", call->src, "identifer was not declared as a function" },
			                    {{ "note", ast->src, "declared here" }});

		call->fdef = ast;
	}
	
	// resolve a type identifier from any relevant scope
	AST_type* resolve_type (SourceRange const& ident_src) {
		// min_scope = 0, functions can call all types visible to them
		AST* ast = resolve_ident(ident_src, ident_src.text(), 0);
		if (ast->kind != A_TYPE)
			throw CompilerExcept({ "error", ident_src, "identifer was not declared as a type" },
			                    {{ "note", ast->src, "declared here" }});

		return (AST_type*)ast;
	}
};

struct SemanticAnalysis {
	AST_Module& modl;

	IdentiferStack stack;

	std::vector<AST_funcdef*>   funcs_stack;

	void semantic_analysis (AST_block* root) {
		for (auto* f : BUILTIN_FUNCS)
			stack.declare_ident(f, f->ident);

		for (auto* t : BASIC_TYPES)
			stack.declare_ident(t, t->ident);

		{ // add a declaration for a main function (global space of the file itself represents the main function)
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF);
			module_main->ident = "main";
			module_main->ret_struct = nullptr;
			module_main->ret_struct_ty = nullptr;
			module_main->args = {};
			module_main->body = root;
			module_main->src = root->src;

			modl.funcs.emplace_back(module_main);

			funcs_stack.emplace_back(module_main);
		}

		recurse(root);

		assert(funcs_stack.size() == 1);
		funcs_stack.pop_back();
	}
	
	// declare struct and func idents and resolve their members/args to enable use of structs/funcs before they are declared
	// NOTE: don't declare the member or arg idents yet, since they are not local variables and the callargs/struct members are found through seperate means
	void prescan_block (AST_block* block) {
		// only funcs/structs in this scope
		smallvec<AST_structdef*, 32> local_structs;
		smallvec<AST_funcdef*, 32>   local_funcs;

		for (auto* ast : block->statements) {
			if (ast->kind == A_FUNCDEF) {
				auto* fdef = (AST_funcdef*)ast;

				stack.declare_ident(fdef, fdef->ident);

				modl.funcs.emplace_back(fdef);
				local_funcs.push(fdef);

				if (fdef->ret_struct)
					modl.structs.emplace_back(fdef->ret_struct);
			}
			else if (ast->kind == A_STRUCTDEF) {
				auto* struc = (AST_structdef*)ast;

				auto* type = ast_alloc<AST_type>(A_TYPE);
				type->tclass = TY_STRUCT;
				type->ident  = struc->ident;
				type->decl   = struc;
				type->src    = struc->src;

				stack.declare_ident(type, struc->ident);

				modl.structs.emplace_back(struc);
				local_structs.push(struc);
			}
		}
		
		// seperate pass for structs to enable:
		/*
			a : A;
			a.b.f = 5.0;
			struct A {
				b : B;
			}
			struct B {
				f : flt;
			}
		*/
		for (auto* struc : local_structs) {
			for (auto* member : struc->members) {
				auto type_ident = member->typeexpr;
				assert(type_ident.start);
				member->type = Typeref::LValue( stack.resolve_type(type_ident) );
			}
		}
		// seperate pass for funcs to enable:
		/*
			b : B;
			foo(b);
			func (b : B) {
				
			}
			struct B {
				f : flt;
			}
		*/
		for (auto* fdef : local_funcs) {
			resolve_funcdecl_args(fdef->args, true);
			resolve_funcdecl_args(fdef->rets, false);
		}
	}

	// TODO: Could implement these by dispatching to a set of rules within each AST_type
	//  that specify how operators behave, since this is likely how I would need to implement
	//  operator overloading for custom types as well (which I intent to support for vector libs / containers etc.)
	// Custom types with operator overloading (always structs?) would be found during ident prescan
	//  after which point we would know which operators were overloaded and what types those operators require/return
	
	Typeref typecheck_unary_op (AST_unop* op, AST* operand) {
		if (operand->type.ty == nullptr)
			ERROR(operand->src, "void is not a valid operand");

		auto* ty = operand->type.ty;

		if (op->op == OP_LOGICAL_NOT) {
			// TODO: !<non-bool> should be transformed (in ast?) to !(bool)<non-bool>
			if (ty != pTY_BOOL)
				ERROR(op->src, "logical not (!x) is not valid for type");

			return Typeref::RValue(pTY_BOOL);
		}

		if (op->op == OP_INC || op->op == OP_DEC) {
			// TODO: allow this for custom types?
			if (operand->type.rval)
				ERROR(operand->src, "cannot inc/decrement RValue");
		}

		switch (ty->tclass) {
		case TY_INT: {
			switch (op->op) {
				case OP_POSITIVE: // no-op
				case OP_NEGATE: 
				case OP_BIT_NOT:
				case OP_INC:    
				case OP_DEC:    
					return Typeref::RValue(ty);

				INVALID_DEFAULT;
			}
		}
		case TY_BOOL: {
			switch (op->op) {
				case OP_BIT_NOT:
					return Typeref::RValue(ty);

				case OP_POSITIVE: ERROR(op->src, "positive operator is not valid for bool");
				case OP_NEGATE:   ERROR(op->src, "negate is not valid for bool"           );
				case OP_INC:      ERROR(op->src, "increment is not valid for bool"        );
				case OP_DEC:      ERROR(op->src, "decrement is not valid for bool"        );

				INVALID_DEFAULT;
			}
		}
		case TY_FLT: {
			switch (op->op) {
				case OP_POSITIVE:
				case OP_NEGATE:  
					return Typeref::RValue(ty);

				case OP_BIT_NOT: ERROR(op->src, "bitwise operators not valid for floats");

				// NOTE: Maybe this is a weird decision, but incrementing a float rarely makes sense unlike ints
				// Furthermore unlike for ints there is no inc/dec instruction in the cpu either, so maybe don't define this operator for floats
				case OP_INC: ERROR(op->src, "increment is not valid for floats (are you sure you want a float?)");
				case OP_DEC: ERROR(op->src, "decrement is not valid for floats (are you sure you want a float?)");

				INVALID_DEFAULT;
			}
		}
		case TY_STR: {
			ERROR(op->src, "operator valid for strings");
		}
		INVALID_DEFAULT;
		}
	}
	Typeref typecheck_binary_op (AST_binop* op, AST* lhs, AST* rhs) {
		if (lhs->type.ty == nullptr)
			ERROR(lhs->src, "void is not a valid operand");
		if (rhs->type.ty == nullptr)
			ERROR(rhs->src, "void is not a valid operand");
		
		if (lhs->type.ty->tclass == TY_STRUCT)
			ERROR(op->src, "operator not valid for struct");

		if (op->op == OP_LOGICAL_AND || op->op == OP_LOGICAL_OR) {
			// TODO: <non-bool> with and or or should be each transformed (in ast?) to (bool)<non-bool>
			if (lhs->type.ty != pTY_BOOL)
				ERROR(lhs->src, "logical not (!x) is not valid for type");
			if (rhs->type.ty != pTY_BOOL)
				ERROR(rhs->src, "logical not (!x) is not valid for type");

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

			return Typeref::RValue(pTY_BOOL);
		}
		
		// TOOD: there might be operators for which this is not true
		if (lhs->type.ty != rhs->type.ty)
			ERROR(op->src, "binary operator: types do not match");

		switch (lhs->type.ty->tclass) {
		case TY_INT: {
			switch (op->op) {
				case OP_ADD:
				case OP_SUB:
				case OP_MUL:
				case OP_DIV:
				case OP_MOD:
					return Typeref::RValue(lhs->type.ty);
				
				case OP_BIT_AND:
				case OP_BIT_OR: 
				case OP_BIT_XOR:
					return Typeref::RValue(lhs->type.ty);
				
				case OP_LESS:      
				case OP_LESSEQ:    
				case OP_GREATER:   
				case OP_GREATEREQ: 
				case OP_EQUALS:    
				case OP_NOT_EQUALS:
					return Typeref::RValue(pTY_BOOL);

				INVALID_DEFAULT;
			}
		} break;
		case TY_BOOL: {
			switch (op->op) {
				case OP_ADD:
				case OP_SUB:
				case OP_MUL:
				case OP_DIV:
				case OP_MOD:
					ERROR(op->src, "math ops not valid for this type");
		
				case OP_BIT_AND:
				case OP_BIT_OR: 
				case OP_BIT_XOR:
					return Typeref::RValue(lhs->type.ty);
				
				case OP_LESS:
				case OP_LESSEQ:
				case OP_GREATER:
				case OP_GREATEREQ:
					ERROR(op->src, "can't compare bools like that");
		
				case OP_EQUALS:    
				case OP_NOT_EQUALS:
					return Typeref::RValue(pTY_BOOL);

				INVALID_DEFAULT;
			}
		} break;
		case TY_FLT: {
			switch (op->op) {
				case OP_ADD:
				case OP_SUB:
				case OP_MUL:
				case OP_DIV:
					return Typeref::RValue(lhs->type.ty);

				case OP_MOD:
					ERROR(op->src, "remainder operator not valid for floats");

				case OP_BIT_AND:
				case OP_BIT_OR:
				case OP_BIT_XOR:
					ERROR(op->src, "bitwise operators not valid for floats");
				
				// always ordered comparisons (NaN behavior)
				case OP_LESS:
				case OP_LESSEQ:
				case OP_GREATER:
				case OP_GREATEREQ:
				case OP_EQUALS:
				case OP_NOT_EQUALS:
					return Typeref::RValue(pTY_BOOL);

				INVALID_DEFAULT;
			}
		} break;

		default:
			ERROR(op->src, "math ops not valid for this type");
		}
	}
	
	Typeref resolve_member_op (AST_binop* op, AST* lhs, AST* rhs) {
		
		if (!lhs->type.ty || lhs->type.ty->tclass != TY_STRUCT)
			ERROR(lhs->src, "member operator '.' expects struct on the left-hand side");
		if (rhs->kind != A_VAR)
			ERROR(rhs->src, "member operator '.' expects member identifier on the right-hand side");
			
		auto find_member = [] (AST_structdef* struc, AST_var* memb) -> AST_vardecl* {
			for (auto* strucmem : struc->members) {
				if (strucmem->ident == memb->ident)
					return strucmem;
			}

			ERROR(memb->src, "struct member not found");
		};

		auto* struc = (AST_structdef*)lhs->type.ty->decl;
		auto* memb = (AST_var*)rhs;

		memb->decl = find_member(struc, memb);
		memb->type = Typeref::LValue(memb->decl->type.ty);

		// a.b is a RValue if a is a RValue, b is always just a identifier referring to a member of the a struct refers to
		Typeref ty;
		ty.ty   = memb->type.ty;
		ty.rval = lhs->type.rval;
		return ty;
	}
			
	void typecheck_assignment (AST_binop* op, AST* lhs, AST* rhs) {
		if (rhs->type.ty == nullptr) {
			// everything on the rhs of assignments except calls with void return should have a non-void type
			assert(rhs->kind == A_CALL);

			ERROR(op->src, "assignment: can't assign void to something"); // TODO: specify what is void
		}

		if (lhs->type.rval)
			ERROR(op->src, "assignment: cannot assign to a RValue"); // TODO: specify what is RValue

		if (op->op == OP_ASSIGN) {
			// check if types match
			if (lhs->type.ty != rhs->type.ty)
				ERROR(op->src, "assignment: types do not match"); // specify both sides of the assignment (without 2 notes?)
		}
		else {
			auto res_ty = typecheck_binary_op(op, lhs, rhs);
			if (res_ty.ty != lhs->type.ty)
				ERROR(op->src, "compount assignment: result type of operator is not the same as the lhs type");
		}
	}

////
	void resolve_vardecl (AST_vardecl* vardecl, bool is_arg=false) {

		auto type_ident = vardecl->typeexpr;
		if (type_ident.start) {
			vardecl->type.ty = stack.resolve_type(type_ident);
		}

		if (vardecl->init) {

			recurse(vardecl->init);

			if (vardecl->init->type.ty == nullptr) {
				// everything on the rhs of assignments except calls with void return should have a non-void type
				assert(vardecl->init->kind == A_CALL);

				ERROR(vardecl->init->src, "variable initialization: void is not a valid variable type");
			}

			// variable declaration without explicit type -> infer type
			if (vardecl->type.ty == nullptr) {
				vardecl->type.ty = vardecl->init->type.ty;
			}
			// variable declaration with explicit type -> check if types match
			else {
				if (vardecl->type.ty != vardecl->init->type.ty)
					ERROR(vardecl->init->src, "variable initialization: types do not match");
			}
		}

		// function arguments are immutable, thus RValues
		vardecl->type.rval = is_arg;

		assert(vardecl->type.ty);
	}

	void resolve_funcdecl_args (arrview<AST_vardecl*> args, bool is_arg) {
		
		bool default_args = false;
		
		for (size_t i=0; i<args.count; ++i) {
			auto& arg = args[i];

			if (arg->kind == A_VARARGS) {
				if (i != args.count-1)
					ERROR(arg->src, "variadic argument can only appear on the end of the argument list");
				break;
			}

			if (arg->init) {
				default_args = true;
			}
			else {
				if (default_args)
					ERROR(arg->src, "default arguments can only appear after all positional arguments");
			}

			resolve_vardecl(arg, is_arg);
		}
	}
	
	void resolve_callarg (AST_callarg* callarg, AST_vardecl* declarg) {
		recurse(callarg->expr);
		
		callarg->type = callarg->expr->type;

		if (declarg->kind != A_VARARGS && callarg->type.ty != declarg->type.ty)
			ERROR(callarg->src, "argument type mismatch");

		callarg->decl = declarg;
	}
	void resolve_call_args (AST* op, arrview<AST_callarg*> callargs, arrview<AST_vardecl*> declargs) {
		size_t non_vararg_count = 0;
		if (declargs.count > 0)
			non_vararg_count = declargs[declargs.count-1]->kind == A_VARARGS ? declargs.count-1 : declargs.count;

		struct Arg {
			AST_vardecl* decl; // funcdef argdecl this callarg is matched to
			AST*         expr;
		};
		smallvec<Arg, 32> args(non_vararg_count);

		//// collect decl args and start them out not provided

		for (size_t i = 0; i < declargs.count; ++i) {
			if (op->kind == A_RETURN && declargs[i]->kind == A_VARARGS)
				ERROR(declargs[i]->src, "variadic argument no allowed for return values");

			if (declargs[i]->kind == A_VARARGS)
				break;

			args[i].decl = declargs[i];
			args[i].expr = nullptr;
		}

		//// positional args
		size_t i = 0;

		for (; i < callargs.count && callargs[i]->ident.empty(); ++i) {
			if (i >= declargs.count) // no more args in func
				ERROR(callargs[i]->src, "too many arguments");

			if (declargs[i]->kind == A_VARARGS) {
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
				ERROR(callargs[i]->src, "too many arguments");

			if (declargs[i]->kind == A_VARARGS) {
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
			ERROR(callarg->src, "unknown argument");
		};
		
		for (; i < callargs.count; ++i) {
			
			if (callargs[i]->ident.empty())
				ERROR(callargs[i]->src, "named arguments can only appear after all positional arguments");

			size_t argi = find_named_arg(callargs[i]);

			if (args[argi].decl->kind == A_VARARGS)
				ERROR(callargs[i]->src, "variadic arguments cannot be assigned directly");

			if (args[argi].expr)
				throw CompilerExcept({ "error", callargs[i]->src, "argument already set in call" }, {
				                     { "note", args[argi].expr->src, "argument was set here" }});
				                     //{ args[argi].decl->src, "note: argument declaration" }});
			
			resolve_callarg(callargs[i], args[argi].decl);

			args[argi].expr = callargs[i]->expr;
		}

		if (op->kind == A_RETURN) {
			// return args can be set like variables outside of the return statement
			// thus we can't check if they are actually provided
		}
		else {
			assert(op->kind == A_CALL);
			auto* call = (AST_call*)op;

			auto* resolved_args = g_allocator.alloc_array<AST*>(args.count);
			call->resolved_args = { resolved_args, args.count };

			// check that all callargs are provided
			for (size_t i=0; i<args.count; ++i) {
				if (args[i].expr == nullptr) {
					if (args[i].decl->init == nullptr)
						throw CompilerExcept({ "error", call->src, "required argument not provided" },
						                    {{ "note", args[i].decl->src, "argument declaration" }});
					
					args[i].expr = args[i].decl->init;
				}

				resolved_args[i] = args[i].expr;
			}
		}
	}

	void condition_expr (AST* ast) {
		recurse(ast);
		
		if (ast->type.ty != pTY_BOOL)
			ERROR(ast->src, "condition expression must be a bool");
	}

	void recurse (AST* ast) {
		switch (ast->kind) {

		case A_LITERAL: {
			auto* lit = (AST_literal*)ast;
		} break;

		case A_VARDECL: {
			auto* vardecl = (AST_vardecl*)ast;

			stack.declare_ident(vardecl, vardecl->ident);
			
			resolve_vardecl(vardecl);
		} break;

		case A_VAR: {
			auto* var = (AST_var*)ast;
			stack.resolve_var(var);

			ast->type = var->decl->type;
		} break;

		case A_ASSIGNOP: {
			auto* op = (AST_binop*)ast;
			recurse(op->lhs);
			recurse(op->rhs);

			if (op->lhs->kind == A_EXPR_LIST || op->rhs->kind == A_EXPR_LIST) {
				
				if (op->op != OP_ASSIGN)
					ERROR(op->src, "compound assigment not allowed with tuple syntax");

				auto get_elems = [] (AST* ast) -> arrview<AST*> {
					if (ast->kind == A_EXPR_LIST) {
						return ((AST_expr_list*)ast)->expressions;
					}
					else {
						assert(ast->type.ty->tclass == TY_STRUCT);
						auto memb = ((AST_structdef*)ast->type.ty->decl)->members;
						return { (AST**)memb.data, memb.count };
					}
				};
				auto l = get_elems(op->lhs);
				auto r = get_elems(op->rhs);

				size_t count = std::min(l.count, r.count);
				for (size_t i=0; i<count; ++i) {
					
					typecheck_assignment(op, l[i], r[i]);
				}

				if (l.count != r.count)
					ERROR(op->src, "tuple assignment: cannot assign {} values to {} expressions", r.count, l.count);
			}
			else {
				typecheck_assignment(op, op->lhs, op->rhs);
			}
		} break;

		case A_EXPR_LIST: {
			auto* expr_list = (AST_expr_list*)ast;

			for (auto* expr: expr_list->expressions)
				recurse(expr);

			// TODO: create tuple type here?
		} break;

		case A_UNOP: {
			auto* op = (AST_unop*)ast;

			recurse(op->operand);

			op->type = typecheck_unary_op(op, op->operand);
		} break;

		case A_BINOP: {
			auto* op = (AST_binop*)ast;

			recurse(op->lhs);
			
			if (op->op != OP_MEMBER) {
				recurse(op->rhs);

				op->type = typecheck_binary_op(op, op->lhs, op->rhs);
			}
			else {
				// Don't for recurse(op->rhs);
				// since for '.' operator the rhs expected to be A_VAR, but identifier is not resolved like a normal var

				op->type = resolve_member_op(op, op->lhs, op->rhs);
			}
		} break;

		case A_IF:
		case A_SELECT: {
			auto* aif = (AST_if*)ast;

			condition_expr(aif->cond);
			recurse(aif->if_body);
			if (aif->else_body) recurse(aif->else_body);

			if (ast->kind == A_SELECT) {
				if (aif->if_body->type.ty != aif->else_body->type.ty)
					ERROR(aif->src, "select expression: types do not match");
				
				// result is an LValue if both a and b are LValues, so that we can do
				// (cond ? a : b) = 5;
				aif->type.ty = aif->if_body->type.ty;
				aif->type.rval = aif->if_body->type.rval || aif->else_body->type.rval;
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
			prescan_block(block);

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

			prescan_block(block); // can call functions before they are declared from anywhere inside the block

			for (auto* n : block->statements)
				recurse(n);

			stack.reset_scope(old_scope);
		} break;
			
		case A_STRUCTDEF: {
			auto* struc = (AST_structdef*)ast;

			// skip structdef, since members were already resolved during prescan
		} break;

		case A_FUNCDEF: {
			auto* fdef = (AST_funcdef*)ast;

			funcs_stack.emplace_back(fdef);
			auto func_scope = stack.push_scope(true);

			// function args/rets already resolved during prescan, only declare the identifiers for the code body
			for (auto* arg : fdef->args)
				stack.declare_ident(arg, arg->ident);
			for (auto* ret : fdef->rets)
				stack.declare_ident(ret, ret->ident);

			// Don't just recurse into the body block, since that creates another scope, which would allow local vars to shadow args/rets, which is not what we want
			
			prescan_block(fdef->body); // can call functions before they are declared from anywhere inside the block

			for (auto* n : fdef->body->statements)
				recurse(n);

			stack.reset_scope(func_scope);
			funcs_stack.pop_back();
		} break;

		case A_CALL: {
			auto* call = (AST_call*)ast;

			stack.resolve_func_call(call);
			auto* fdef = (AST_funcdef*)call->fdef;

			resolve_call_args(call, call->args, fdef->args);

			// returns are always RValues
			if (fdef->rets.count == 0)
				call->type = {};
			else if (fdef->rets.count == 1)
				call->type = Typeref::RValue( fdef->rets[0]->type.ty );
			else
				call->type = Typeref::RValue( fdef->ret_struct_ty );
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
					
void semantic_analysis (AST_Module& modl) {
	ZoneScoped;

	SemanticAnalysis sem { modl };
	sem.semantic_analysis(modl.ast);
}