#pragma once
#include "common.hpp"
#include "parser.hpp"

namespace IR {

enum IROpType {
	OP_NONE=0,

	//// Bools
	// binary
	OP_b_EQ,
	OP_b_NEQ,
	// unary
	OP_b_NOT,

	//// Ints
	// binary
	OP_i_ADD,
	OP_i_SUB,
	OP_i_MUL,
	OP_i_DIV,
	OP_i_REMAIND,

	OP_i_LT,
	OP_i_LE,
	OP_i_GT,
	OP_i_GE,
	OP_i_EQ,
	OP_i_NEQ,

	// unary
	OP_i_NEG,
	OP_i_NOT,
	OP_i_INC,
	OP_i_DEC,

	//// Floats
	// binary
	OP_f_ADD,
	OP_f_SUB,
	OP_f_MUL,
	OP_f_DIV,

	OP_f_LT,
	OP_f_LE,
	OP_f_GT,
	OP_f_GE,
	OP_f_EQ,
	OP_f_NEQ,

	// unary
	OP_f_NEG,
};
inline const char* IROpType_str[] = {
	"",

	//// Bools
	// binary
	"b_EQ",
	"b_NEQ",
	// unary
	"b_NOT",

	//// Ints
	// binary
	"i_ADD",
	"i_SUB",
	"i_MUL",
	"i_DIV",
	"i_REMAIND",

	"i_LT",
	"i_LE",
	"i_GT",
	"i_GE",
	"i_EQ",
	"i_NEQ",

	// unary
	"i_NEGATE",
	"i_NOT",
	"i_INC",
	"i_DEC",

	//// Floats
	// binary
	"f_ADD",
	"f_SUB",
	"f_MUL",
	"f_DIV",

	"f_LT",
	"f_LE",
	"f_GT",
	"f_GE",
	"f_EQ",
	"f_NEQ",

	// unary
	"f_NEG",
};

IROpType unary2ir (AST* ast, OpType op, Type type) {
	switch (op) {
		case OP_POSITIVE: {
			switch (type) {
				case INT: return OP_NONE; // no-op
				case FLT: return OP_NONE; // no-op
				default: throw CompilerExcept{"error: positive operator is not valid for type", ast->src_tok->source};
			}
		}
		case OP_NEGATE: {
			switch (type) {
				case INT: return OP_i_NEG;
				case FLT: return OP_f_NEG;
				default: throw CompilerExcept{"error: negate is not valid for type", ast->src_tok->source};
			}
		}
		case OP_NOT: {
			switch (type) {
				case INT : return OP_i_NOT;
				case BOOL: return OP_b_NOT;
				default: throw CompilerExcept{"error: not is not valid for type", ast->src_tok->source};
			}
		}
		case OP_INC: {
			switch (type) {
				case INT : return OP_i_INC;
				default: throw CompilerExcept{"error: increment is not valid for type", ast->src_tok->source};
			}
		}
		case OP_DEC: {
			switch (type) {
				case INT : return OP_i_DEC;
				default: throw CompilerExcept{"error: decrement is not valid for type", ast->src_tok->source};
			}
		}
		INVALID_DEFAULT;
	}
}
IROpType binary2ir (AST* ast, OpType op, Type type) {
	switch (type) {
		case INT: {
			switch (op) {
				case OP_ADD:        return OP_i_ADD;
				case OP_SUB:        return OP_i_SUB;
				case OP_MUL:        return OP_i_MUL;
				case OP_DIV:        return OP_i_DIV;
				case OP_REMAINDER:  return OP_i_REMAIND;
				case OP_LESS:       return OP_i_LT;
				case OP_LESSEQ:     return OP_i_LE;
				case OP_GREATER:    return OP_i_GT;
				case OP_GREATEREQ:  return OP_i_GE;
				case OP_EQUALS:     return OP_i_EQ;
				case OP_NOT_EQUALS: return OP_i_NEQ;
				INVALID_DEFAULT;
			}
		} break;
		case BOOL: {
			switch (op) {
				case OP_ADD:        return OP_i_ADD;
				case OP_SUB:        return OP_i_SUB;
				case OP_MUL:        return OP_i_MUL;
				case OP_DIV:        return OP_i_DIV;
				case OP_REMAINDER:  return OP_i_REMAIND;
					throw CompilerExcept{ "error: math ops not valid for this type", ast->src_tok->source };

				case OP_LESS:       return OP_i_LT;
				case OP_LESSEQ:     return OP_i_LE;
				case OP_GREATER:    return OP_i_GT;
				case OP_GREATEREQ:  return OP_i_GE;
					throw CompilerExcept{ "error: can't compare bools like that", ast->src_tok->source };

				case OP_EQUALS:     return OP_b_EQ; 
				case OP_NOT_EQUALS: return OP_b_NEQ;
				INVALID_DEFAULT;
			}
		} break;
		case FLT: {
			switch (op) {
				case OP_REMAINDER:
					throw CompilerExcept{ "error: remainder operator not valid for floats", ast->src_tok->source };
				case OP_ADD:        return OP_f_ADD;
				case OP_SUB:        return OP_f_SUB;
				case OP_MUL:        return OP_f_MUL;
				case OP_DIV:        return OP_f_DIV;
				case OP_LESS:       return OP_f_LT;
				case OP_LESSEQ:     return OP_f_LE;
				case OP_GREATER:    return OP_f_GT;
				case OP_GREATEREQ:  return OP_f_GE;
				case OP_EQUALS:     return OP_f_EQ;
				case OP_NOT_EQUALS: return OP_f_NEQ;
				INVALID_DEFAULT;
			}
		} break;
		default:
			throw CompilerExcept{ "error: math ops not valid for this type", ast->src_tok->source };
	}
}

	
enum VarType : uint8_t {
	VT_UNDEFINED=0, // unused operand
	VT_CONST,     // operand is a constant (codegen will try to turn into into a imm if possible)
	VT_TEMPID,    // operand is id of a temporary generated in expressions
	VT_VARID,     // operand is id of a variable actually declared in the AST

	VT_LABELID,   // operand is id of a jump target, ie. a label
	VT_ARGID,     // operand is id of a function argument or return value
};
struct Var {
	VarType  type;
	size_t   id;
};

enum NodeType : uint8_t {
	DEAD,     // eliminated by dead code elimination

	LABEL,    // used to mark places the jumps go to

	MOVE,     // dst = lhs
	UNOP,     // dst = <op from ast->type> applied to lhs
	BINOP,    // dst = lhs <op from ast->type> rhs

	PUSH_RET, // push return value slot for function (needed to simplify argument stack locations computation)
	PUSH_ARG, // push argument for function
	CALL,     // call function with arguments
	GET_RET,  // get return value by id

	JUMP,     // unconditional jump to lhs
	JUMP_CT,  // conditional jump to dst if rhs true
	JUMP_CF,  // conditional jump to dst if rhs false

	RETURN,   // unconditional jump to function epilouge
};
static inline constexpr const char* Type_str[] = {
	"-",

	"LABEL",

	"MOVE",
	"UNOP",
	"BINOP",

	"PUSH_RET",
	"PUSH_ARG",
	"CALL",
	"GET_RET",

	"JUMP",
	"JUMP_CT",
	"JUMP_CF",

	"RETURN",
};

struct Instruction {
	NodeType type;
	IROpType op;

	Var  dst = { VT_UNDEFINED, 0 }; // dst.id is label id for jumps (type ignored)
	Var  lhs = { VT_UNDEFINED, 0 };
	Var  rhs = { VT_UNDEFINED, 0 };

	AST* ast = nullptr; // ast the IR came from
};

struct IR {

	size_t var_count   = 0;
	size_t temp_count  = 0;
	size_t label_count = 0;
	
	struct LabelInfo {
		AST*        ast;
		const char* name;
		size_t      code_lbl_id;
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

	size_t emit (NodeType type, AST* ast=nullptr, Var dst={}, Var lhs={}, Var rhs={}, IROpType optype=OP_NONE) {
		auto& instr = code.emplace_back();
		instr.type = type;
		instr.op  = optype;
		instr.dst = dst;
		instr.lhs = lhs;
		instr.rhs = rhs;
		instr.ast = ast;
		return (size_t)(&instr - code.data());
	}

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
		auto print_var = [] (Var var, bool pad=true) {
			std::string str;
			switch (var.type) {
				case VT_UNDEFINED: str = ""; break;
				case VT_CONST:     str = prints("%llx",    var.id); break;
				case VT_TEMPID:    str = prints("_t%llu",  var.id); break;
				case VT_VARID:     str = prints("v%llu",   var.id); break;
				case VT_LABELID:   str = prints("L%llu",   var.id); break;
				case VT_ARGID:     str = prints("arg%llu", var.id); break;
			}
			printf(pad ? " %16s" : "%s", str.c_str());
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

	std::vector<AST_funcdef*>& funcdefs;
	std::vector<IR> func_irs;

	struct LoopLabels {
		size_t cont;
		size_t end;
	};
	std::vector<LoopLabels> loop_lbls;
	size_t                  return_lbl;

	void generate () {
		ZoneScoped;

		func_irs.resize(funcdefs.size());

		for (size_t funcid = 0; funcid < funcdefs.size(); ++funcid) {
			auto& func = funcdefs[funcid];
			func->codegen_funcid = funcid;

			func_irs[funcid] = IRgen_funcdef(func);
		}
	}

	IR IRgen_funcdef (AST_funcdef* func) {
		ZoneScoped;

		IR ir;
		ir.code.reserve(1024 * 8);
		loop_lbls.reserve(16);

		loop_lbls.clear();

		size_t retid = 0;
		for (auto* ret=(AST_vardecl*)func->rets; ret != nullptr; ret = (AST_vardecl*)ret->next) {
			Var v = { VT_ARGID, retid++ };
			ret->var_id     = v.id;
			ret->is_arg = true;

			if (ret->init) {
				assert(ret->type != A_VARARGS);

				assert(ret->init->type == A_LITERAL);
				Var val = IRgen(ir, ret->init);

				ir.emit(MOVE, ret, v, val);
			}
		}

		size_t argid = 0;
		for (auto* arg=(AST_vardecl*)func->args; arg != nullptr; arg = (AST_vardecl*)arg->next) {
			Var v = { VT_ARGID, argid++ };
			arg->var_id     = v.id;
			arg->is_arg = true;
		}

		return_lbl = ir.create_label((AST*)func, "return");

		//emit(STK_PUSH, func->body);

		for (auto* n=func->body; n != nullptr; n = n->next)
			IRgen(ir, n);

		//emit(STK_POP, func->body);

		ir.emit(LABEL, (AST*)func, { VT_LABELID, return_lbl });
		ir.emit(RETURN);

		return ir;
	}

	Var IRgen (IR& ir, AST* ast) {
		switch (ast->type) {

			case A_LITERAL: {
				Var tmp = { VT_TEMPID, ir.temp_count++ };
				ir.emit(MOVE, ast, tmp, { VT_CONST, *(size_t*)&((AST_literal*)ast)->value });
				return tmp;
			}

			case A_VARDECL: {
				auto* var = (AST_vardecl*)ast;

				Var decl = { VT_VARID, ir.var_count++ };
				var->var_id = decl.id;
				var->is_arg = false;

				if (var->init) {
					Var val = IRgen(ir, var->init);

					ir.emit(MOVE, ast, decl, val);
				}
				return {};
			}

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* vardecl = (AST_vardecl*)var->decl;
				return { vardecl->is_arg ? VT_ARGID : VT_VARID, vardecl->var_id };
			}
			
			case A_UNOP: {
				auto* op = (AST_unop*)ast;

				auto optype = unary2ir(ast, op->op, ast->valtype);

				switch (op->op) {
					case OP_POSITIVE:
					case OP_NEGATE: case OP_NOT: {
						Var operand = IRgen(ir, op->operand);

						if (op->op == OP_POSITIVE)
							return operand; // unary + is a no-op

						Var tmp = { VT_TEMPID, ir.temp_count++ };

						ir.emit(UNOP, ast, tmp, operand, {}, optype);
						return tmp;
					}

					case OP_INC: case OP_DEC: {
						assert(op->operand->type == A_VAR);
						auto* oper_var = (AST_var*)op->operand;
						auto* oper_decl = (AST_vardecl*)oper_var->decl;

						Var var = { VT_VARID, oper_decl->var_id };

						Var tmp = { VT_TEMPID, ir.temp_count++ };
						ir.emit(MOVE, ast, tmp, var); // copy old var value
						ir.emit(UNOP, ast, var, var, {}, optype); // inc/dec var
						return tmp;
					}
				}
			}

			case A_BINOP: {
				auto* op = (AST_binop*)ast;

				assert(op->lhs->valtype == op->rhs->valtype);
				auto optype = binary2ir(ast, op->op, op->lhs->valtype);

				Var lhs = IRgen(ir, op->lhs);
				Var rhs = IRgen(ir, op->rhs);

				assert(op->lhs->valtype == op->rhs->valtype);

				Var tmp = { VT_TEMPID, ir.temp_count++ };
				ir.emit(BINOP, ast, tmp, lhs, rhs, optype);
				return tmp;
			}

			case A_ASSIGNOP: {
				auto* op = (AST_binop*)ast;

				if (op->op == OP_ASSIGN) {
					auto* op = (AST_binop*)ast;

					Var dst = IRgen(ir, op->lhs);
					Var var = IRgen(ir, op->rhs);

					ir.emit(MOVE, ast, dst, var);
					return {};
				}
				else {
					assert(op->lhs->valtype == op->rhs->valtype);
					auto optype = binary2ir(ast, op->op, op->lhs->valtype);

					assert(op->lhs->type == A_VAR);
					auto* lhs_var = (AST_var*)op->lhs;
					auto* lhs_decl = (AST_vardecl*)lhs_var->decl;

					Var var = { VT_VARID, lhs_decl->var_id };
					Var rhs = IRgen(ir, op->rhs);

					ir.emit(BINOP, ast, var, var, rhs, optype);
					return {};
				}
			}

			case A_BLOCK: {
				auto* block = (AST_block*)ast;

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					IRgen(ir, n);
				}

				return {};
			}

			case A_IF:
			case A_SELECT: {
				auto* aif = (AST_if*)ast;

				Var tmp = {};
				if (ast->type == A_SELECT)
					tmp = { VT_TEMPID, ir.temp_count++ };

				size_t else_lbl = ir.create_label(ast, "else");

				// condition
				Var cond = IRgen(ir, aif->cond);
				ir.emit(JUMP_CF, ast, { VT_LABELID, else_lbl }, cond);

				// if body
				Var val = IRgen(ir, aif->if_body);
				if (ast->type == A_SELECT)
					ir.emit(MOVE, ast, tmp, val);

				// no else body
				if (!aif->else_body) {
					ir.emit(LABEL, ast, { VT_LABELID, else_lbl });
				}
				// else body
				else {
					size_t endif_lbl = ir.create_label(ast, "endif");

					ir.emit(JUMP, ast, { VT_LABELID, endif_lbl });
					ir.emit(LABEL, ast, { VT_LABELID, else_lbl });

					// false body
					Var val = IRgen(ir, aif->else_body);
					if (ast->type == A_SELECT)
						ir.emit(MOVE, ast, tmp, val);

					ir.emit(LABEL, ast, { VT_LABELID, endif_lbl });
				}

				return tmp;
			}

			case A_WHILE:
			case A_DO_WHILE:
			case A_FOR: {
				auto* loop = (AST_loop*)ast;

				// start
				if (loop->start)
					IRgen(ir, loop->start);

				size_t loop_lbl     = ir.create_label(ast, "loop");     // for loop itself, jumps to top of loop
				size_t loopcont_lbl = ir.create_label(ast, "loopcont"); // for continue,    jumps to end of body
				size_t end_lbl      = ir.create_label(ast, "loopend");  // for break,       jumps to end of loop

				ir.emit(LABEL, ast, { VT_LABELID, loop_lbl });

				loop_lbls.push_back({ loopcont_lbl, end_lbl });

				if (ast->type == A_DO_WHILE) {
					// body
					IRgen(ir, loop->body);
					ir.emit(LABEL, ast, { VT_LABELID, loopcont_lbl });

					// condition
					Var cond = IRgen(ir, loop->cond);
					ir.emit(JUMP_CT, ast, { VT_LABELID, loop_lbl }, cond);

					assert(!loop->end);
				}
				else {
					// condition
					Var cond = IRgen(ir, loop->cond);
					ir.emit(JUMP_CF, ast, { VT_LABELID, end_lbl }, cond);

					// body
					IRgen(ir, loop->body);
					ir.emit(LABEL, ast, { VT_LABELID, loopcont_lbl });

					// end
					if (loop->end)
						IRgen(ir, loop->end);

					// unconditional jump to loop top
					ir.emit(JUMP, ast, { VT_LABELID, loop_lbl });
				}

				ir.emit(LABEL, ast, { VT_LABELID, end_lbl });

				loop_lbls.pop_back();
				return {};
			}

			case A_CALL: {
				auto* call = (AST_call*)ast;
				auto* fdef = (AST_funcdef*)call->fdef;

				// collect function args
				struct Argdecl {
					AST_vardecl* decl;
					bool         set;
					size_t       callarg;
				};
				std::vector<Argdecl> declargs;
				declargs.reserve(32);

				for (auto* arg = (AST_vardecl*)fdef->args; arg != nullptr; arg = (AST_vardecl*)arg->next) {
					declargs.push_back({ arg, false, (size_t)-1 });
				}

				// generate IR for callargs first (move into temps)
				std::vector<Var> callargs;
				callargs.reserve(32);

				bool vararg     = false;
				size_t vararg_i = 0;

				size_t calli = 0;
				for (auto* arg = (AST_callarg*)call->args; arg != nullptr; arg = (AST_callarg*)arg->next) {
					callargs.push_back( IRgen(ir, arg->expr) );

					if (arg->decl->type == A_VARARGS) {
						vararg = true;
						vararg_i = calli;
					}

					declargs[arg->decli].set = true;
					declargs[arg->decli].callarg = calli++;
				}

				// only after the IR of all callargs is done generate PUSH_RET & PUSH_ARG so we don't mix two calls
				// (arg ids will clash and the stack space for the args will be clobbered)
				size_t ret_count = 0;
				for (auto* ret = (AST*)fdef->rets; ret != nullptr; ret = ret->next) {
					size_t retid = ret_count++;

					ir.emit(PUSH_RET, ret, { VT_ARGID, retid });
				}

				size_t arg_count = 0;

				// push call args
				{
					auto arg = declargs.begin();

					while (arg != declargs.end() && arg->decl->type != A_VARARGS) {
						// set provided call argument
						if (arg->set) {
							auto& val = callargs[arg->callarg];

							ir.emit(PUSH_ARG, arg->decl, { VT_ARGID, arg_count++ }, val);
						}
						// set default call argument
						else {
							assert(arg->decl->type != A_VARARGS);
							assert(arg->decl->init && arg->decl->init->type == A_LITERAL);

							Var val = IRgen(ir, arg->decl->init);

							ir.emit(PUSH_ARG, arg->decl, { VT_ARGID, arg_count++ }, val);
						}

						arg++;
					}

					// handle varargs
					if (vararg) {
						assert(arg->decl->type == A_VARARGS);

						for (size_t i=vararg_i; i<callargs.size(); ++i) {
							auto& val = callargs[i];

							ir.emit(PUSH_ARG, arg->decl, { VT_ARGID, arg_count++ }, val);
						}
					}
				}

				ir.max_retargs = std::max(ir.max_retargs, ret_count + arg_count);

				ir.emit(CALL, ast);

				// get return value
				Var tmp = {};

				if (ret_count > 0) {
					tmp = { VT_TEMPID, ir.temp_count++ };
					ir.emit(GET_RET, fdef->rets, tmp, { VT_ARGID, 0 });
				}

				return tmp;
			}

			case A_RETURN: {
				auto* ret = (AST_return*)ast;

				for (auto* arg = (AST_callarg*)ret->args; arg != nullptr; arg = (AST_callarg*)arg->next) {
					auto val = IRgen(ir, arg->expr);

					Var retv = { VT_ARGID, arg->decli };

					ir.emit(MOVE, arg, retv, val);
				}

				ir.emit(JUMP, ast, { VT_LABELID, return_lbl });
				return {};
			}

			case A_BREAK: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: break not inside of any loop", ast->src_tok->source};
				
				ir.emit(JUMP, ast, { VT_LABELID, loop_lbls.back().end });
				return {};
			}
			case A_CONTINUE: {
				if (loop_lbls.empty())
					throw CompilerExcept{"error: continue not inside of any loop", ast->src_tok->source};

				ir.emit(JUMP, ast, { VT_LABELID, loop_lbls.back().cont });
				return {};
			}

			case A_FUNCDEF:
			default:
				return {};
		}
	}
};

}
