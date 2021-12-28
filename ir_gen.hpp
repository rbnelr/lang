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
			throw CompilerExcept{"error: identifer already declared in this scope", ast->tok->source}; // TODO: print declaration of that identifer
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
		throw CompilerExcept{"error: unknown identifer", node->tok->source};
	}
	void resolve_var (AST_var* var) {
		// min_scope = func_scope_id, functions can only access their own variables
		AST* ast = resolve_ident((AST*)var, var->ident, cur_scope.func_scope_id);
		if (ast->type != A_VARDECL)
			throw CompilerExcept{"error: identifer was not declared as variable", var->a.tok->source};

		var->decl = (AST_vardecl*)ast;
	}

	void resolve_func_call (AST_call* call) {
		// min_scope = 0, functions can call all functions visible to them
		AST* ast = resolve_ident((AST*)call, call->ident, 0);
		if (!(ast->type == A_FUNCDEF || ast->type == A_FUNCDEF_BUILTIN))
			throw CompilerExcept{"error: identifer was not declared as function", call->a.tok->source};
		
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
			throw CompilerExcept{"error: too many arguments to function", call->a.tok->source};

		if (declarg->type == A_VARARGS) {
			// last func arg is varargs, any number of remaining call args match (including 0)
			assert(!declarg->next);
			return;
		}

		assert(declarg->type == A_VARDECL);

		if (!callarg) // no args left in call
			throw CompilerExcept{"error: too few arguments to function", call->a.tok->source};

		if (callarg->valtype != declarg->valtype)
			throw CompilerExcept{"error: call argument type mismatch", callarg->tok->source};

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
			AST_funcdef* module_main = ast_alloc<AST_funcdef>(A_FUNCDEF, root->tok);
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
							throw CompilerExcept{"error: assignment: can't assign void return type", op->a.tok->source};
						}
						op->lhs->valtype = op->rhs->valtype;
					}
					else {
						if (op->lhs->valtype != op->rhs->valtype)
							throw CompilerExcept{"error: variable declaration assignment: types do not match", op->a.tok->source};
					}
				}
				else {
					if (op->lhs->valtype != op->rhs->valtype)
						throw CompilerExcept{"error: assignment: types do not match", op->a.tok->source};
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
					throw CompilerExcept{"error: binary operator: types do not match", op->a.tok->source};
				
				op->a.valtype = op->lhs->valtype;
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				visit(node, [this] (AST* node) { recurse(node); });

				if (node->type == A_SELECT) {
					if (aif->if_body->valtype != aif->else_body->valtype)
						throw CompilerExcept{"error: select expression: types do not match", aif->a.tok->source};
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

struct IR_Var {
	bool     is_temp : 1;
	size_t   id      : 63;
};

struct IR {
	enum Type {
		LABEL,    // used to mark places jumps go to

		STK_PUSH, // Create a new scope
		STK_POP,  // Close the last scope, making stack space be reused (even without optimizations)

		//VARDECL,  // dst

		MOVE,     // dst = lhs
		CONST,    // dst = ((AST_literal*)ast)->value
		UNOP,     // dst = <op from ast->type> applied to lhs
		BINOP,    // dst = lhs <op from ast->type> rhs

		ARG_PUSH, // push lhs as argument for next call
		ARG_POP,  // pop lhs argument
		CALL,     // call func using pushed args
		
		JUMP,     // unconditional jump to lhs
		JUMP_CT,  // conditional jump to dst if rhs true
		JUMP_CF,  // conditional jump to dst if rhs false

		RETURN,
	};
	static inline constexpr const char* Type_str[] = {
		"LABEL",

		"STK_PUSH",
		"STK_POP",

		//"VARDECL",
		
		"MOVE",
		"CONST",
		"UNOP",
		"BINOP",

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

		IR_Var dst; // dst var  or  dst.id = dst label

		union {
			struct {
				IR_Var lhs;
				IR_Var rhs;
			};
			struct {
				size_t const_val;
			};
		};

		// only for debug info and debug printing
		union {
			AST*        ast; // ast the IR came from
			const char* name; // for labels: label name
		};
	};

	size_t next_var = 0;
	size_t next_temp = 0;
	size_t next_label = 0;

	std::vector<Instruction> code;

	void dbg_print () {
		printf("--------------------------------------------------------------------------------\n");
		printf("IR code :\n");

		auto print_ast = [] (Instruction& instr) {
			switch (instr.type) {
				case CONST: {
					assert(instr.ast->type == A_LITERAL);
					auto* lit = (AST_literal*)instr.ast;
		
					::dbg_print(lit->a.valtype, lit->value);
				} break;
		
				default: {
					if (instr.ast) {
						if (instr.ast->type == A_VARDECL || instr.ast->type == A_VARARGS) {
							auto* var = (AST_vardecl*)instr.ast;
							printf("%.*s", (int)var->ident.size(), var->ident.data());
						}
						else {
							auto text  = get_source(instr.ast).text();
							auto escaped = escape_string_capped(text, 40);
		
							fputs(escaped.c_str(), stdout); // use fputs rather than printf since we might print printf-codes
						}
					}
				} break;
			}
		};
		auto print_var = [] (IR_Var var) {
			if (var.is_temp) printf(" %7s", prints("_t%llu", var.id).c_str());
			else             printf(" %7s", prints("r%llu", var.id).c_str());
		};
		auto print_lbl = [] (IR_Var var) {
			printf(" %7s", prints("L%llu", var.id).c_str());
		};

		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			bool dst=false, lhs=false, rhs=false;
			bool jump=false;

			switch (instr.type) {
				case LABEL:    dst = true;             break;

				case STK_PUSH:                         break;
				case STK_POP:                          break;

				case MOVE:     dst = true; lhs = true; break;
				case CONST:    dst = true;             break;
				case UNOP:     dst = true; lhs = true; break;
				case BINOP:    dst = true; lhs = true; rhs = true; break;

				case ARG_PUSH: dst = true; lhs = true; break;
				case ARG_POP:  dst = true;             break;
				case CALL:                             break;

				case JUMP:     dst = true;             jump = true; break;
				case JUMP_CT:  dst = true; lhs = true; jump = true; break;
				case JUMP_CF:  dst = true; lhs = true; jump = true; break;
				case RETURN:   dst = true;             break;
			}

			printf("%5lli |", i);

			if (instr.type == LABEL) {
				printf("%s(%lli):\n", instr.name, instr.dst.id);
			}
			else {
				printf("  %-10s", Type_str[instr.type]);

				if (jump && dst) {
					print_lbl(instr.dst);
				}
				else {
					if (dst) print_var(instr.dst);
					else     printf(" %7s", "");
				}

				//if (lhs) printf(" =");
				//else     printf("  ");

				if (lhs) print_var(instr.lhs);
				else     printf(" %7s", "");
				
				if (rhs) print_var(instr.rhs);
				else     printf(" %7s", "");

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
		size_t repeat;
		size_t end;
	};
	std::vector<LoopLabels> loop_lbls;
	size_t                  return_lbl;

	struct FuncArg {
		AST* all;
		AST* decl;
	};
	std::vector<FuncArg> arg_stack;

	size_t emit (IR::Type type, AST* ast=nullptr, IR_Var dst={0}, IR_Var lhs={0}, IR_Var rhs={0}) {
		auto& instr = ir.code.emplace_back();
		instr.type = type;
		instr.dst = dst;
		instr.lhs = lhs;
		instr.rhs = rhs;
		instr.ast = ast;
		return (size_t)(&instr - ir.code.data());
	}
	size_t emit (IR::Type type, char const* name, size_t lbl_id) {
		auto& instr = ir.code.emplace_back();
		instr.type = type;
		instr.dst = { 0, lbl_id };
		instr.lhs = {0};
		instr.rhs = {0};
		instr.name = name;
		return (size_t)(&instr - ir.code.data());
	}
	size_t emit (IR::Type type, AST* ast, size_t lbl_id, IR_Var jmp_cond={0}) {
		auto& instr = ir.code.emplace_back();
		instr.type = type;
		instr.dst = { 0, lbl_id };
		instr.lhs = jmp_cond;
		instr.rhs = {0,0};
		instr.ast = ast;
		return (size_t)(&instr - ir.code.data());
	}
	size_t emit_const (IR::Type type, AST* ast, IR_Var dst, size_t const_val) {
		auto& instr = ir.code.emplace_back();
		instr.type = type;
		instr.dst = dst;
		instr.const_val = const_val;
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

		return_lbl = ir.next_label++;

		//emit(IR::STK_PUSH, func->body);

		for (auto* n=func->body; n != nullptr; n = n->next)
			IRgen(n);

		//emit(IR::STK_POP, func->body);

		emit(IR::LABEL, "return", return_lbl);
		emit(IR::RETURN);
	}

	IR_Var IRgen (AST* ast, AST_vardecl* dst=nullptr, size_t dst_stk_loc=0) {
		switch (ast->type) {

			case A_LITERAL: {
				IR_Var tmp = { 1, ir.next_temp++ };
				emit_const(IR::CONST, ast, tmp, *(size_t*)&((AST_literal*)ast)->value);
				return tmp;
			}

			case A_VARDECL: {
				auto* var = (AST_vardecl*)ast;

				IR_Var v = { 0, ir.next_var++ };
				var->var_id = v.id;
				return v;
			}

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* vardecl = (AST_vardecl*)var->decl;
				return { 0, vardecl->var_id };
			}

			case A_NEGATE: case A_NOT: {
				auto* op = (AST_unop*)ast;

				IR_Var operand = IRgen(op->operand);

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

				IR_Var tmp = { 1, ir.next_temp++ };
				emit(IR::UNOP, ast, tmp, operand);
				return tmp;
			}

			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)ast;

				assert(op->operand->type == A_VAR);
				auto* oper_var = (AST_var*)op->operand;
				auto* oper_decl = (AST_vardecl*)oper_var->decl;

				IR_Var var = { 0, oper_decl->var_id };

				IR_Var tmp = { 1, ir.next_temp++ };
				emit(IR::MOVE, ast, tmp, var); // copy old var value
				emit(IR::UNOP, ast, var, var); // inc/dec var
				return tmp;
			}

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)ast;

				IR_Var lhs = IRgen(op->lhs);
				IR_Var rhs = IRgen(op->rhs);

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

				IR_Var tmp = { 1, ir.next_temp++ };
				emit(IR::BINOP, ast, tmp, lhs, rhs);
				return tmp;
			}

			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: case A_REMAINDEREQ: {
				auto* op = (AST_binop*)ast;

				assert(op->lhs->type == A_VAR);
				auto* lhs_var = (AST_var*)op->lhs;
				auto* lhs_decl = (AST_vardecl*)lhs_var->decl;

				IR_Var var = { 0, lhs_decl->var_id };
				IR_Var rhs = IRgen(op->rhs);

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

				emit(IR::BINOP, ast, var, var, rhs);
				return {};
			}

			case A_ASSIGN: {
				auto* op = (AST_binop*)ast;

				IR_Var dst = IRgen(op->lhs);
				IR_Var var = IRgen(op->rhs);

				emit(IR::MOVE, ast, dst, var);
				return {};
			}

			case A_BLOCK: {
				auto* block = (AST_block*)ast;

				emit(IR::STK_PUSH, ast);

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					IRgen(n);
				}

				emit(IR::STK_POP, ast);
				return {};
			}

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)ast;

				size_t else_lbl = ir.next_label++;

				// condition
				IR_Var cond = IRgen(aif->cond);
				emit(IR::JUMP_CF, ast, else_lbl, cond);

				// if body
				IRgen(aif->if_body);

				// no else body
				if (!aif->else_body) {
					emit(IR::LABEL, "else", else_lbl);
				}
				// else body
				else {
					size_t end_lbl = ir.next_label++;

					emit(IR::JUMP, ast, end_lbl);
					emit(IR::LABEL, "else", else_lbl);

					// false body
					IRgen(aif->else_body);

					emit(IR::LABEL, "if-end", end_lbl);
				}
				return {};
			}

			case A_LOOP: {
				auto* loop = (AST_loop*)ast;

				emit(IR::STK_PUSH); // stk for loop header variable

				// start
				IRgen(loop->start);

				size_t loop_lbl = ir.next_label++;
				size_t end_lbl  = ir.next_label++;

				emit(IR::LABEL, "loop", loop_lbl);

				loop_lbls.push_back({ loop_lbl, end_lbl });

				// condition
				IR_Var cond = IRgen(loop->cond);
				emit(IR::JUMP_CF, ast, end_lbl, cond);
				
				// body
				IRgen(loop->body);

				// end
				IRgen(loop->end);

				// unconditional jump to loop top
				emit(IR::JUMP, ast, loop_lbl);

				emit(IR::LABEL, "loop-end", end_lbl);

				emit(IR::STK_POP); // stk for loop header variable
				loop_lbls.pop_back();
				return {};
			}

			case A_CALL: {
				auto* call = (AST_call*)ast;
				auto* fdef = (AST_funcdef*)call->fdef;

				auto* argdecl = fdef->decl.args;
				for (auto* arg = call->args; arg != nullptr; arg = arg->next) {
					size_t argid = arg_stack.size();
					
					IR_Var arg_ir = IRgen(arg);
					emit(IR::ARG_PUSH, argdecl, { 0, argid }, arg_ir);

					arg_stack.push_back({ arg, argdecl });
					
					if (argdecl->type != A_VARARGS) argdecl = argdecl->next;
				}

				emit(IR::CALL, ast);

				// get return arg

				for (size_t argid = arg_stack.size(); argid>0; ) {
					argid--;

					emit(IR::ARG_POP, arg_stack[argid].decl, { 0, argid });
				}
				arg_stack.clear();

				return {};
			}

			case A_RETURN: {
				emit(IR::JUMP, ast, return_lbl);
				return {};
			}
			case A_BREAK: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: break not inside of any loop", ast->tok->source};
				
				emit(IR::JUMP, ast, loop_lbls.back().end);
				return {};
			}
			case A_CONTINUE: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: continue not inside of any loop", ast->tok->source};

				emit(IR::JUMP, ast, loop_lbls.back().repeat);
				return {};
			}

			case A_FUNCDEF:
			default:
				return {};
		}
	}
};
