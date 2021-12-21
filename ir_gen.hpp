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

struct IdentResolver {
	IdentiferStack stack;

	std::vector<AST_funcdef*> funcs;

	void resolve (AST* root) {
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
					throw CompilerExcept{"error: binary operator: types do not match", op->a.source};
				
				op->a.valtype = op->lhs->valtype;
			} break;

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)node;

				visit(node, [this] (AST* node) { recurse(node); });

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

struct IRGen {

	struct IRNode {
		enum Type {
			LABEL,    // no-op, but used to mark places jumps go to
			STK_PUSH, // Create a new scope
			STK_POP,  // Close the last scope, making stack space be reused

			VARDECL, // var decl lhs and rhs are null
			MOVE,    // assign rhs to lhs, lhs is a VARDECL node

			CONST,   // get value from ((AST_literal*)ast)->value
			UNOP,    // only uses lhs     op is ast->type
			BINOP,   // uses lhs and rhs  op is ast->type
			
			JUMP,    // unconditional jump to lhs
			JUMP_CT, // conditional jump to lhs if rhs true
			JUMP_CF, // conditional jump to lhs if rhs false

			CALL,    // lhs is nothing for now (but could be computed func ptr) rhs is ptr to _array_ of returns + args,  # of rets is ast->decl->retc, # of args is  ast->argc
			RETURN,
		};
		Type type;

		AST* ast;

		IRNode* lhs;
		IRNode* rhs;
	};
	std::vector<IRNode*> code;

	struct LoopLabels {
		IRNode* repeat;
		IRNode* end;
	};
	std::vector<LoopLabels> loop_lbls;
	IRNode*                 return_lbl;

	IRNode* alloc (IRNode::Type type, AST* ast=nullptr, IRNode* lhs=nullptr, IRNode* rhs=nullptr) {
		auto* ptr = g_allocator.alloc<IRNode>();
		*ptr = { type, ast, lhs, rhs };
		return ptr;
	}
	void add (IRNode* node) {
		assert(node);
		code.emplace_back(node);
	}

	IRNode* emit (IRNode::Type type, AST* ast=nullptr, IRNode* lhs=nullptr, IRNode* rhs=nullptr) {
		auto* ptr = alloc(type, ast, lhs, rhs);
		add(ptr);
		return ptr;
	}

	IRNode* emit_n (size_t count, IRNode::Type type) {
		auto* ptr = g_allocator.alloc_array<IRNode>(count);
		
		size_t idx = code.size();
		code.resize(idx + count);
		
		for (size_t i=0; i<count; ++i) {
			ptr[i].type = type;
			ptr[i].ast = nullptr;
			ptr[i].lhs = nullptr;
			ptr[i].rhs = nullptr;
			code[idx+i] = &ptr[i];
		}
		return ptr;
	}

	void generate (std::vector<AST_funcdef*>& funcdefs) {
		code.reserve(1024);

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

		return_lbl = alloc(IRNode::LABEL);

		emit(IRNode::STK_PUSH);

		for (auto* n=func->body; n != nullptr; n = n->next)
			IRgen(n);

		emit(IRNode::STK_POP);

		add(return_lbl);
		emit(IRNode::RETURN);
	}

	IRNode* IRgen (AST* ast, AST_vardecl* dst=nullptr, size_t dst_stk_loc=0) {
		switch (ast->type) {

			case A_LITERAL:
				return emit(IRNode::CONST, ast);
			case A_VARDECL: {
				auto* vardecl = (AST_vardecl*)ast;

				auto* node = emit(IRNode::VARDECL, ast);
				vardecl->IRnode = node;
				return node;
			}

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* vardecl = (AST_vardecl*)var->decl;
				return (IRNode*)vardecl->IRnode;
			}

			case A_ASSIGN: {
				auto* op = (AST_binop*)ast;

				auto* lhs = IRgen(op->lhs);
				auto* rhs = IRgen(op->rhs);

				emit(IRNode::MOVE, ast, lhs, rhs);
				return nullptr;
			}

			case A_BLOCK: {
				auto* block = (AST_block*)ast;

				emit(IRNode::STK_PUSH);

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					IRgen(n);
				}

				emit(IRNode::STK_POP);
				return nullptr;
			}

			case A_CALL: {
				auto* call = (AST_call*)ast;

				auto* fdef = (AST_funcdef*)call->fdef;

				size_t count = fdef->decl.retc + call->argc;
				auto* ret_args = emit_n(count, IRNode::VARDECL);

				auto* cur = ret_args;
				for (auto* ret_ast = fdef->decl.rets; ret_ast != nullptr; ret_ast = ret_ast->next) {
					auto* ret = cur++;
					ret->ast = ret_ast;
				}

				auto* farg = fdef->decl.args;
				for (auto* arg_ast = call->args; arg_ast != nullptr; arg_ast = arg_ast->next) {
					auto* argdecl = (AST_vardecl*)farg;
					
					auto* arg_ir = IRgen(arg_ast);

					auto* arg_decl = cur++;
					arg_decl->ast = (AST*)argdecl;

					emit(IRNode::MOVE, arg_ast, arg_decl, arg_ir);

					if (farg->type != A_VARARGS) farg = farg->next;
				}

				emit(IRNode::CALL, ast, nullptr, ret_args);

				if (fdef->decl.retc == 0)
					return nullptr;
				return &ret_args[0];
			}

			case A_ADDEQ: case A_SUBEQ: case A_MULEQ: case A_DIVEQ: case A_REMAINDEREQ: {
				auto* op = (AST_binop*)ast;
				assert(op->lhs->type == A_VAR);
				auto* lhs = (AST_var*)op->lhs;

				auto* lhs_ir = (IRNode*)((AST_vardecl*)lhs->decl)->IRnode;
				auto* rhs = IRgen(op->rhs);

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

				auto* op_ir = emit(IRNode::BINOP, ast, lhs_ir, rhs);
				emit(IRNode::MOVE, ast, lhs_ir, op_ir);
				return nullptr;
			}

			case A_ADD: case A_SUB: case A_MUL: case A_DIV: case A_REMAINDER:
			case A_LESS: case A_LESSEQ: case A_GREATER: case A_GREATEREQ:
			case A_EQUALS: case A_NOT_EQUALS: {
				auto* op = (AST_binop*)ast;

				auto* lhs = IRgen(op->lhs);
				auto* rhs = IRgen(op->rhs);

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

				return emit(IRNode::BINOP, ast, lhs, rhs);
			} break;

			case A_NEGATE: case A_NOT: {
				auto* op = (AST_unop*)ast;

				auto* operand_ir = IRgen(op->operand);

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

				return emit(IRNode::UNOP, ast, operand_ir);
			}

			case A_INC: case A_DEC: {
				auto* op = (AST_unop*)ast;
				assert(op->operand->type == A_VAR);
				auto* operand_ir = (IRNode*)((AST_vardecl*)((AST_var*)op->operand)->decl)->IRnode;

				auto* op_ir = emit(IRNode::UNOP, ast, operand_ir);
				emit(IRNode::MOVE, ast, operand_ir, op_ir);
				return op_ir;
			}

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)ast;

				auto* false_lbl = alloc(IRNode::LABEL);

				// condition
				auto* cond = IRgen(aif->cond);
				emit(IRNode::JUMP_CF, ast, false_lbl, cond);

				// true body
				IRgen(aif->true_body);

				// no false body
				if (!aif->false_body) {
					add(false_lbl);
				}
				// false body
				else {
					auto* end_lbl = alloc(IRNode::LABEL);

					emit(IRNode::JUMP, ast, end_lbl);
					add(false_lbl);

					// false body
					IRgen(aif->false_body);

					add(end_lbl);
				}
				return nullptr;
			}

			case A_LOOP: {
				auto* loop = (AST_loop*)ast;

				emit(IRNode::STK_PUSH); // stk for loop header variable

				// start
				IRgen(loop->start);

				auto* loop_lbl = emit(IRNode::LABEL);
				auto* end_lbl  = alloc(IRNode::LABEL);

				loop_lbls.push_back({ loop_lbl, end_lbl });

				// condition
				auto* cond = IRgen(loop->cond);
				emit(IRNode::JUMP_CF, ast, end_lbl, cond);
				
				// body
				IRgen(loop->body);

				// end
				IRgen(loop->end);

				// unconditional jump to loop top
				emit(IRNode::JUMP, ast, loop_lbl, cond);

				add(end_lbl);

				emit(IRNode::STK_POP); // stk for loop header variable
				loop_lbls.pop_back();
				return nullptr;
			}

			case A_RETURN: {
				emit(IRNode::JUMP, ast, return_lbl);
				return nullptr;
			}
			case A_BREAK: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: break not inside of any loop", ast->source};
				
				emit(IRNode::JUMP, ast, loop_lbls.back().end);
				return nullptr;
			}
			case A_CONTINUE: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: continue not inside of any loop", ast->source};

				emit(IRNode::JUMP, ast, loop_lbls.back().repeat);
				return nullptr;
			}

			case A_FUNCDEF:
			default:
				return nullptr;
		}
	}
};
