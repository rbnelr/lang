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
		throw CompilerExcept{"error: unknown identifer", node->src_tok->source};
	}
	void resolve_var (AST_var* var) {
		// min_scope = func_scope_id, functions can only access their own variables
		AST* ast = resolve_ident((AST*)var, var->ident, cur_scope.func_scope_id);
		if (ast->type != A_VARDECL)
			throw CompilerExcept{"error: identifer was not declared as variable", var->a.src_tok->source};

		var->decl = (AST_vardecl*)ast;
	}

	void resolve_func_call (AST_call* call) {
		// min_scope = 0, functions can call all functions visible to them
		AST* ast = resolve_ident((AST*)call, call->ident, 0);
		if (!(ast->type == A_FUNCDEF || ast->type == A_FUNCDEF_BUILTIN))
			throw CompilerExcept{"error: identifer was not declared as function", call->a.src_tok->source};
		
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
			throw CompilerExcept{"error: too many arguments to function", call->a.src_tok->source};

		if (declarg->type == A_VARARGS) {
			// last func arg is varargs, any number of remaining call args match (including 0)
			assert(!declarg->next);
			return;
		}

		assert(declarg->type == A_VARDECL);

		if (!callarg) // no args left in call
			throw CompilerExcept{"error: too few arguments to function", call->a.src_tok->source};

		if (callarg->valtype != declarg->valtype)
			throw CompilerExcept{"error: call argument type mismatch", callarg->src_tok->source};

		declarg = declarg->next;
		callarg = callarg->next;
	}
}

struct IdentResolver {
	IdentiferStack stack;

	std::vector<AST_funcdef*> funcs;

	void resolve (AST* root) {
		for (AST_funcdef_builtin const* f : BUILTIN_FUNCS)
			stack.declare_func((AST_funcdef*)f); // cast is safe

		{
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF, root->src_tok);
			module_main->decl.ident = "main";
			module_main->decl.retc = 0;
			module_main->decl.rets = nullptr;
			module_main->decl.argc = 0;
			module_main->decl.args = nullptr;
			module_main->body = root;

			funcs.emplace_back(module_main);
		}

		recurse(root);
	}

	void recurse (AST* node) {
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
				recurse(op->lhs);
				recurse(op->rhs);

				if (op->lhs->type == A_VARDECL) {
					assert(node->type == A_ASSIGN);

					if (op->lhs->valtype == VOID) {
						if (op->rhs->valtype == VOID) {
							assert(op->rhs->type == A_CALL); // everything on the rhs of assignments except calls should have a resolved type
							throw CompilerExcept{"error: assignment: can't assign void return type", op->a.src_tok->source};
						}
						op->lhs->valtype = op->rhs->valtype;
					}
					else {
						if (op->lhs->valtype != op->rhs->valtype)
							throw CompilerExcept{"error: variable declaration assignment: types do not match", op->a.src_tok->source};
					}
				}
				else {
					if (op->lhs->valtype != op->rhs->valtype)
						throw CompilerExcept{"error: assignment: types do not match", op->a.src_tok->source};
				}
			} break;

			case A_CALL: {
				auto* call = (AST_call*)node;
				stack.resolve_func_call(call);

				for (auto* n=call->args; n != nullptr; n = n->next)
					recurse(n);

				auto* funcdef = (AST_funcdef*)call->fdef;
				match_call_args(call, funcdef->decl);
				call->a.valtype = funcdef->decl.retc > 0 ? funcdef->decl.rets->valtype : VOID;
			} break;

			case A_NEGATE: case A_NOT:
			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)node;
				recurse(op->operand);
				op->a.valtype = op->operand->valtype;
			} break;

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)node;
				recurse(op->lhs);
				recurse(op->rhs);

				if (op->lhs->valtype != op->rhs->valtype)
					throw CompilerExcept{"error: binary operator: types do not match", op->a.src_tok->source};
				
				op->a.valtype = op->lhs->valtype;
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				visit(node, [this] (AST* node) { recurse(node); });

				if (node->type == A_SELECT) {
					if (aif->if_body->valtype != aif->else_body->valtype)
						throw CompilerExcept{"error: select expression: types do not match", aif->a.src_tok->source};
					aif->a.valtype = aif->if_body->valtype;
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
				visit(node, [this] (AST* node) { recurse(node); });

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
};

namespace IR {

enum VarType : uint8_t {
	UNDEFINED=0,
	T_CONST,
	TEMP,
	VAR,

	T_LABEL,
	T_ARG,
};
struct Var {
	VarType  type;
	size_t   id;
};

enum Type : uint8_t {
	DEAD,     // eliminated by dead code elimination

	LABEL,    // used to mark places the jumps go to

	VARDECL,  // needed to be able to control variable stack placement out of order (compared to first assignment) (NOTE that temporaries are always in order)

	MOVE,     // dst = lhs
	UNOP,     // dst = <op from ast->type> applied to lhs
	BINOP,    // dst = lhs <op from ast->type> rhs

	RET_PUSH, // 
	RET_POP,  // 
	ARG_PUSH, // push lhs as argument for next call
	ARG_POP,  // pop lhs argument
	CALL,     // call func using pushed args

	JUMP,     // unconditional jump to lhs
	JUMP_CT,  // conditional jump to dst if rhs true
	JUMP_CF,  // conditional jump to dst if rhs false

	RETURN,   // unconditional jump to function epilouge
};
static inline constexpr const char* Type_str[] = {
	"-",

	"LABEL",

	"VARDECL",

	"MOVE",
	"UNOP",
	"BINOP",

	"RET_PUSH",
	"RET_POP",
	"ARG_PUSH",
	"ARG_POP",
	"CALL",

	"JUMP",
	"JUMP_CT",
	"JUMP_CF",

	"RETURN",
};

struct Instruction {
	Type type;

	Var  dst = { UNDEFINED, 0 }; // dst.id is label id for jumps (type ignored)

	Var  lhs = { UNDEFINED, 0 };
	Var  rhs = { UNDEFINED, 0 };

	AST* ast = nullptr; // ast the IR came from
};


struct IR {

	size_t var_count = 0;
	size_t temp_count = 0;
	size_t label_count = 0;
	
	struct LabelInfo {
		AST*        ast;
		const char* name;
		size_t      codeaddr;
	};
	std::vector<LabelInfo> labels; // labels by id (ids are in order of label references, ie. jumps)

	size_t create_label (AST* ast, char const* type) {
		size_t lbl_id = label_count++;

		grow(labels, label_count);
		labels[lbl_id].ast = ast;
		labels[lbl_id].name = format("%s(%lli)", type, lbl_id);

		return lbl_id;
	}

	size_t max_retargs = 0;

	std::vector<Instruction> code;

	void dbg_print () {
		printf("--------------------------------------------------------------------------------\n");
		printf("IR code :\n");

		auto print_ast = [] (Instruction& instr) {
			if (instr.ast) {
				if (instr.ast->type == A_VARDECL || instr.ast->type == A_VARARGS) {
					auto* var = (AST_vardecl*)instr.ast;
					printf("%.*s", (int)var->ident.size(), var->ident.data());
				}
				else {
					auto text  = instr.ast->src_tok->source.text();
					auto escaped = escape_string_capped(text, 40);
		
					fputs(escaped.c_str(), stdout); // use fputs rather than printf since we might print printf-codes
				}
			}
		};
		auto print_var = [] (Var var) {
			std::string str;
			switch (var.type) {
				case UNDEFINED:  str = ""; break;
				case T_CONST:   str = prints("%llx",   var.id); break;
				case TEMP:       str = prints("_t%llu", var.id); break;
				case VAR:        str = prints("r%llu",  var.id); break;
				case T_LABEL:    str = prints("L%llu",  var.id); break;
				case T_ARG:      str = prints("%llu",   var.id); break;
			}
			printf(" %16s", str.c_str());
		};

		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			printf("%5llu |", i);

			if (instr.type == LABEL) {
				printf("%s:\n", labels[instr.dst.id].name);
			}
			else {
				printf("  %-10s", Type_str[instr.type]);

				print_var(instr.dst);
				print_var(instr.lhs);
				print_var(instr.rhs);

				printf("  # ");

				print_ast(instr);

				printf("\n");
			}
		}
	}
};

struct IRGen {

	IR ir;

	struct LoopLabels {
		size_t cont;
		size_t end;
	};
	std::vector<LoopLabels> loop_lbls;
	size_t                  return_lbl;

	struct FuncArg {
		AST* call;
		AST* decl;
	};
	std::vector<AST*>      ret_stack;
	std::vector<FuncArg>   arg_stack;

	size_t emit (Type type, AST* ast=nullptr, Var dst={}, Var lhs={}, Var rhs={}) {
		auto& instr = ir.code.emplace_back();
		instr.type = type;
		instr.dst = dst;
		instr.lhs = lhs;
		instr.rhs = rhs;
		instr.ast = ast;
		return (size_t)(&instr - ir.code.data());
	}

	void generate (std::vector<AST_funcdef*>& funcdefs) {
		ir.code.reserve(1024 * 8);
		loop_lbls.reserve(16);

		auto& func = funcdefs[0];
		//for (auto& func : funcs) {
		IRgen_funcdef(func);
		//}
	}

	void IRgen_funcdef (AST_funcdef* func) {
		ZoneScoped;

		for (auto* n=func->decl.args; n != nullptr; n = n->next) {
			
		}
		for (auto* n=func->decl.rets; n != nullptr; n = n->next) {
			
		}

		return_lbl = ir.create_label((AST*)func, "return");

		//emit(STK_PUSH, func->body);

		for (auto* n=func->body; n != nullptr; n = n->next)
			IRgen(n);

		//emit(STK_POP, func->body);

		emit(LABEL, (AST*)func, { T_LABEL, return_lbl });
		emit(RETURN);
	}

	Var IRgen (AST* ast, AST_vardecl* dst=nullptr, size_t dst_stk_loc=0) {
		switch (ast->type) {

			case A_LITERAL: {
				Var tmp = { TEMP, ir.temp_count++ };
				emit(MOVE, ast, tmp, { T_CONST, *(size_t*)&((AST_literal*)ast)->value });
				return tmp;
			}

			case A_VARDECL: {
				auto* var = (AST_vardecl*)ast;

				Var v = { VAR, ir.var_count++ };
				var->var_id = v.id;

				emit(VARDECL, ast, v);
				return v;
			}

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* vardecl = (AST_vardecl*)var->decl;
				return { VAR, vardecl->var_id };
			}

			case A_NEGATE: case A_NOT: {
				auto* op = (AST_unop*)ast;

				Var operand = IRgen(op->operand);

				/*
				Opcode opc;
				switch (ast->type) {
				case A_NEGATE: {
				switch (op->operand->valtype) {
				case INT: opc = OP_NEG; break;
				case FLT: opc = OP_FNEG; break;
				default: throw CompilerExcept{"error: negate is not valid for type", ast->source};
				}
				} break;
				case A_NOT: {
				switch (op->operand->valtype) {
				case INT : opc = OP_NOT; break;
				case BOOL: opc = OP_NOT; break;
				default: throw CompilerExcept{"error: not is not valid for type", ast->source};
				}
				} break;
				}*/

				Var tmp = { TEMP, ir.temp_count++ };
				emit(UNOP, ast, tmp, operand);
				return tmp;
			}

			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)ast;

				assert(op->operand->type == A_VAR);
				auto* oper_var = (AST_var*)op->operand;
				auto* oper_decl = (AST_vardecl*)oper_var->decl;

				Var var = { VAR, oper_decl->var_id };

				Var tmp = { TEMP, ir.temp_count++ };
				emit(MOVE, ast, tmp, var); // copy old var value
				emit(UNOP, ast, var, var); // inc/dec var
				return tmp;
			}

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)ast;

				Var lhs = IRgen(op->lhs);
				Var rhs = IRgen(op->rhs);

				assert(op->lhs->valtype == op->rhs->valtype);

				/*
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
				}*/

				Var tmp = { TEMP, ir.temp_count++ };
				emit(BINOP, ast, tmp, lhs, rhs);
				return tmp;
			}

			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: case A_REMAINDEREQ: {
				auto* op = (AST_binop*)ast;

				assert(op->lhs->type == A_VAR);
				auto* lhs_var = (AST_var*)op->lhs;
				auto* lhs_decl = (AST_vardecl*)lhs_var->decl;

				Var var = { VAR, lhs_decl->var_id };
				Var rhs = IRgen(op->rhs);

				/*
				if (op->lhs->valtype != op->rhs->valtype)
				throw CompilerExcept{"error: compund assignment operator: types do not match", op->a.source};

				Opcode opc;
				switch (op->lhs->valtype) {
				case INT: opc = OP_ADD; break;
				case FLT: opc = OP_FADD; break;
				default:
				throw CompilerExcept{"error: math ops not valid for this type", ast->source};
				}

				if (op->lhs->valtype == FLT && ast->type == A_REMAINDEREQ)
				throw CompilerExcept{"error: remainder operator not valid for floats", ast->source};
				*/

				emit(BINOP, ast, var, var, rhs);
				return {};
			}

			case A_ASSIGN: {
				auto* op = (AST_binop*)ast;

				Var dst = IRgen(op->lhs);
				Var var = IRgen(op->rhs);

				emit(MOVE, ast, dst, var);
				return {};
			}

			case A_BLOCK: {
				auto* block = (AST_block*)ast;

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					IRgen(n);
				}

				return {};
			}

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)ast;

				Var tmp = {};
				if (ast->type == A_SELECT)
					tmp = { TEMP, ir.temp_count++ };

				size_t else_lbl = ir.create_label(ast, "else");

				// condition
				Var cond = IRgen(aif->cond);
				emit(JUMP_CF, ast, { T_LABEL, else_lbl }, cond);

				// if body
				Var val = IRgen(aif->if_body);
				if (ast->type == A_SELECT)
					emit(MOVE, ast, tmp, val);

				// no else body
				if (!aif->else_body) {
					emit(LABEL, ast, { T_LABEL, else_lbl });
				}
				// else body
				else {
					size_t endif_lbl = ir.create_label(ast, "endif");

					emit(JUMP, ast, { T_LABEL, endif_lbl });
					emit(LABEL, ast, { T_LABEL, else_lbl });

					// false body
					Var val = IRgen(aif->else_body);
					if (ast->type == A_SELECT)
						emit(MOVE, ast, tmp, val);

					emit(LABEL, ast, { T_LABEL, endif_lbl });
				}

				return tmp;
			}

			case A_LOOP: {
				auto* loop = (AST_loop*)ast;

				// start
				IRgen(loop->start);

				size_t loop_lbl     = ir.create_label(ast, "loop");
				size_t loopcont_lbl = ir.create_label(ast, "loopcont");
				size_t end_lbl      = ir.create_label(ast, "loopend");

				emit(LABEL, ast, { T_LABEL, loop_lbl });

				loop_lbls.push_back({ loopcont_lbl, end_lbl });

				// condition
				Var cond = IRgen(loop->cond);
				emit(JUMP_CF, ast, { T_LABEL, end_lbl }, cond);
				
				// body
				IRgen(loop->body);

				emit(LABEL, ast, { T_LABEL, loopcont_lbl });

				// end
				IRgen(loop->end);

				// unconditional jump to loop top
				emit(JUMP, ast, { T_LABEL, loop_lbl });

				emit(LABEL, ast, { T_LABEL, end_lbl });

				loop_lbls.pop_back();
				return {};
			}

			case A_CALL: {
				auto* call = (AST_call*)ast;
				auto* fdef = (AST_funcdef*)call->fdef;

				for (auto* ret = fdef->decl.rets; ret != nullptr; ret = ret->next) {
					size_t retid = ret_stack.size();

					emit(RET_PUSH, ret, { T_ARG, retid });

					ret_stack.push_back(ret);
				}

				auto* argdecl = fdef->decl.args;
				for (auto* arg = call->args; arg != nullptr; arg = arg->next) {
					size_t argid = arg_stack.size();
					
					Var arg_ir = IRgen(arg);
					emit(ARG_PUSH, argdecl, { T_ARG, argid + ret_stack.size() }, arg_ir);

					arg_stack.push_back({ arg, argdecl });
					
					if (argdecl->type != A_VARARGS) argdecl = argdecl->next;
				}

				ir.max_retargs = std::max(ir.max_retargs, ret_stack.size() + arg_stack.size());

				emit(CALL, ast);

				// get return value
				Var tmp = {};

				for (size_t argid = arg_stack.size(); argid>0; ) {
					argid--;

					emit(ARG_POP, arg_stack[argid].decl, { UNDEFINED, 0 }, { T_ARG, argid + ret_stack.size() });
				}
				arg_stack.clear();

				for (size_t retid = ret_stack.size(); retid>0; ) {
					retid--;

					if (retid == 0) {
						tmp = { TEMP, ir.temp_count++ };
						emit(RET_POP, ret_stack[retid], tmp, { T_ARG, retid });
					}
					else {
						emit(RET_POP, ret_stack[retid], { UNDEFINED, 0 }, { T_ARG, retid });
					}
				}
				ret_stack.clear();

				return tmp;
			}

			case A_RETURN: {
				emit(JUMP, ast, { T_LABEL, return_lbl });
				return {};
			}
			case A_BREAK: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: break not inside of any loop", ast->src_tok->source};
				
				emit(JUMP, ast, { T_LABEL, loop_lbls.back().end });
				return {};
			}
			case A_CONTINUE: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: continue not inside of any loop", ast->src_tok->source};

				emit(JUMP, ast, { T_LABEL, loop_lbls.back().cont });
				return {};
			}

			case A_FUNCDEF:
			default:
				return {};
		}
	}
};

}
