#pragma once
#include "common.hpp"
#include "ir_gen.hpp"

enum Opcode : uint32_t {
	OP_IMM = 1u << 31,

	OP_NOP=0,

	OP_MOV,      // MEM, MEM

	OP_PUSH,     // IMM
	OP_POP,      // IMM

	// call bytecode function
	OP_CALL,
	// call builtin (native) function
	OP_CALLB, // dst=func ptr src=frame ptr (at func args)

	OP_RET,

	OP_JMP,     // JUMP DST
	OP_JNZ,     // JUMP DST, COND     jump if nonzero  
	OP_JZ,      // JUMP DST, COND     jump if zero     

	OP_NEG,     // MEM
	OP_NOT,     // MEM
	OP_INC,     // MEM
	OP_DEC,     // MEM

	OP_ADD,	    // MEM, MEM
	OP_SUB,	    // MEM, MEM
	OP_MUL,	    // MEM, MEM
	OP_DIV,	    // MEM, MEM
	OP_REMAIND, // MEM, MEM
	OP_LT,      // MEM, MEM
	OP_LTE,     // MEM, MEM
	OP_GT,      // MEM, MEM
	OP_GTE,     // MEM, MEM
	OP_EQ,      // MEM, MEM
	OP_NEQ,     // MEM, MEM

	OP_FNEG,    // MEM

	OP_FADD,    // MEM, MEM
	OP_FSUB,    // MEM, MEM
	OP_FMUL,    // MEM, MEM
	OP_FDIV,    // MEM, MEM
	_OP_FREMAIND, // dummy
	OP_FLT,      // MEM, MEM
	OP_FLTE,     // MEM, MEM
	OP_FGT,      // MEM, MEM
	OP_FGTE,     // MEM, MEM
	OP_FEQ,      // MEM, MEM
	OP_FNEQ,     // MEM, MEM
};
inline constexpr const char* Opcode_str[] = {
	"NOP",  

	"MOV",  

	"PUSH", 
	"POP", 

	"CALL",
	"CALLB",

	"RET",

	"JMP",
	"JNZ",
	"JZ",

	"NEG",
	"NOT",
	"INC",
	"DEC",     
	"ADD",	      
	"SUB",	      
	"MUL",	      
	"DIV",	      
	"REMAIND",   
	"LT",        
	"LTE",       
	"GT",        
	"GTE",       
	"EQ",        
	"NEQ",       
	"FNEG",      
	"FADD",      
	"FSUB",      
	"FMUL",      
	"FDIV",      
	"_FREMAIND", 
	"FLT",        
	"FLTE",       
	"FGT",        
	"FGTE",       
	"FEQ",        
	"FNEQ",       
};

ENUM_BITFLAG_OPERATORS_TYPE(Opcode, uint32_t)

struct Instruction {
	Opcode  code;
	size_t  dst; // address of destination
	size_t  src; // address of source or immediate 64 bit value

	Instruction (Opcode code, size_t dst=0, size_t src=0): code{code}, dst{dst}, src{src} {}
};

Opcode unary2opcode (AST* ast) {
	switch (ast->type) {
		case A_NEGATE: {
			switch (ast->valtype) {
				case INT: return OP_NEG;
				case FLT: return OP_FNEG;
				INVALID_DEFAULT;
			}
		}
		case A_NOT: {
			switch (ast->valtype) {
				case INT : return OP_NOT;
				case BOOL: return OP_NOT;
				INVALID_DEFAULT;
			}
		}
		case A_INC: {
			switch (ast->valtype) {
				case INT : return OP_INC;
				INVALID_DEFAULT;
			}
		}
		case A_DEC: {
			switch (ast->valtype) {
				case INT : return OP_DEC;
				INVALID_DEFAULT;
			}
		}
		INVALID_DEFAULT;
	}
	return (Opcode)0;
}
Opcode binary2opcode (AST* ast) {
	switch (ast->valtype) {
		case INT: {
			switch (ast->type) {
				case A_ADD: case A_ADDEQ:             return OP_ADD;
				case A_SUB: case A_SUBEQ:             return OP_SUB;
				case A_MUL: case A_MULEQ:             return OP_MUL;
				case A_DIV: case A_DIVEQ:             return OP_DIV;
				case A_REMAINDER: case A_REMAINDEREQ: return OP_REMAIND;
				case A_LESS:                          return OP_LT;
				case A_LESSEQ:                        return OP_LTE;
				case A_GREATER:                       return OP_GT;
				case A_GREATEREQ:                     return OP_GTE;
				case A_EQUALS:                        return OP_EQ;
				case A_NOT_EQUALS:                    return OP_NEQ;
				INVALID_DEFAULT;
			}
		} break;
		case BOOL: {
			switch (ast->type) {
				case A_EQUALS:     return OP_EQ; 
				case A_NOT_EQUALS: return OP_NEQ;
				INVALID_DEFAULT;
			}
		} break;
		case FLT: {
			switch (ast->type) {
				case A_ADD: case A_ADDEQ:             return OP_FADD;
				case A_SUB: case A_SUBEQ:             return OP_FSUB;
				case A_MUL: case A_MULEQ:             return OP_FMUL;
				case A_DIV: case A_DIVEQ:             return OP_FDIV;
				case A_LESS:                          return OP_FLT;
				case A_LESSEQ:                        return OP_FLTE;
				case A_GREATER:                       return OP_FGT;
				case A_GREATEREQ:                     return OP_FGTE;
				case A_EQUALS:                        return OP_FEQ;
				case A_NOT_EQUALS:                    return OP_FNEQ;
				INVALID_DEFAULT;
			}
		} break;
		INVALID_DEFAULT;
	}
	return (Opcode)0;
}

struct Codegen {
	std::vector<Instruction> code;

	struct Info {
		size_t       addr;
		const char*  str;
	};

	std::vector<size_t> labels_ordered; // lbl ids in order of code addr
	std::vector<Info> labels; // lbl id -> lbl code addr + dbg str

	std::vector<Info> istrs;

	std::vector<size_t> vars;
	std::vector<size_t> temps;

	template <typename T>
	void grow (std::vector<T>& vec, size_t min_sz) {
		if (vec.size() < min_sz)
			vec.resize(min_sz);
	}

	void dbg_print () {
		printf("--------------------------------------------------------------------------------\n");
		printf("bytecode:\n");

		auto cur_lbl_id = labels_ordered.begin();
		auto cur_istr = istrs.begin();

		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			while (cur_lbl_id < labels_ordered.end() && labels[*cur_lbl_id].addr == i) {
				printf("       %s:\n", labels[*cur_lbl_id].str);
				cur_lbl_id++;
			}

			bool dst=true, src=true, jmp=false;

			Opcode code  = instr.code & ~OP_IMM;
			bool src_imm = (instr.code & OP_IMM) != 0;

			switch (code) {
				case OP_RET:
					dst = false;
					src = false;
					break;

				case OP_PUSH:
				case OP_POP:
					src = false;
					break;

				case OP_JMP:
					src = false;
					jmp = true;
					break;
				case OP_JNZ:
				case OP_JZ:
					jmp = true;
					break;

				case OP_NEG:
				case OP_NOT:
				case OP_INC:
				case OP_DEC:
				case OP_FNEG:
					src = false;
					break;
			}

			printf("%5llx |", i);

			auto print_operand = [] (size_t val) {
				printf(" %16s", prints("[%llx]", val).c_str());
			};
			auto print_imm = [] (size_t val) {
				printf(" %16llx", val);
			};

			printf("  %-5s", Opcode_str[code]);
			
			if (jmp) {
				print_imm(instr.dst);
			}
			else {
				if (dst)        print_operand(instr.dst);
				else            printf(" %16s", "");
			}

			if (src && src_imm) print_imm(instr.src);
			else if (src)       print_operand(instr.src);
			else                printf(" %16s", "");

			printf("  # ");

			while (cur_istr < istrs.end() && cur_istr->addr == i) {
				printf("%s", cur_istr->str);
				cur_istr++;
			}

			printf("\n");
		}
	}

	void generate (std::vector<IR::Instruction>& ir_code) {
		
		size_t max_vars = 0;
		size_t max_tmps = 0;
		size_t max_args = 0;

		// 1st pass: collect labels and count vars / temps
		for (size_t i=0; i<ir_code.size(); ++i) {
			auto& ir = ir_code[i];

			switch (ir.type) {
				case IR::LABEL: {
					size_t lbl_id = ir.dst.id;

					grow(labels, lbl_id+1);
					labels[lbl_id] = Info{ 0, format("%s(%lli)", ir.name, ir.dst.id) };

					labels_ordered.push_back(lbl_id);
				} break;

				case IR::MOVE:
				case IR::CONST:
				case IR::UNOP:
				case IR::BINOP: {
					//size_t lbl_idx = op.dst.id;
					//
					//grow(labels, lbl_idx+1);
					//labels[lbl_idx] = { 0, op.name };

					if (ir.dst.is_temp) max_tmps = std::max(max_tmps, ir.dst.id+1);
					else                max_vars = std::max(max_vars, ir.dst.id+1);
				} break;

				case IR::ARG_PUSH: {
					max_args = std::max(max_args, ir.dst.id+1);
				} break;
				case IR::ARG_POP: {
					
				} break;
			}
		}

		auto get_addr = [&] (IR_Var var) {
			size_t addr = var.id;
			if (var.is_temp) addr += max_vars;
			return addr;
		};
		auto format_const = [] (AST* ast) {
			auto text = ast->tok->source.text();
			return format("=%.*s", (int)text.size(), text.data());
		};
		auto format_source = [] (AST* ast) {
			auto text = ast->tok->source.text();
			return format("%.*s", (int)text.size(), text.data());
		};
		auto format_jmp = [this] (AST* ast, size_t lbl_id) {
			auto text = ast->tok->source.text();
			return format("%s, %.*s", labels[lbl_id].str, (int)text.size(), text.data());
		};

		size_t stk_sz   = max_vars + max_tmps + max_args;

		{ // code for function prologe
			code.emplace_back(OP_PUSH, stk_sz);
		}

		// 2nd pass: generate code
		for (size_t i=0; i<ir_code.size(); ++i) {
			auto& ir = ir_code[i];

			size_t dst = get_addr(ir.dst);
			size_t lhs = get_addr(ir.lhs);
			size_t rhs = get_addr(ir.rhs);

			switch (ir.type) {
				case IR::LABEL: {
					size_t lbl_id = ir.dst.id;
					labels[lbl_id].addr = code.size();
				} break;

				case IR::STK_PUSH:
				case IR::STK_POP: {
					// no-op
				} break;

				case IR::MOVE: {
					//if (ir.ast && ir.ast->type == A_ASSIGN) {
					//	auto* op = (AST_binop*)ir.ast;
					//	auto* alhs = op->lhs;
					//	istrs.push_back({ code.size(), format_source(alhs) });
					//}
					code.emplace_back(OP_MOV, dst, lhs);
				} break;

				case IR::CONST: {
					istrs.push_back({ code.size(), format_const(ir.ast) });
					code.emplace_back(OP_MOV | OP_IMM, dst, ir.const_val);
				} break;

				case IR::UNOP: {
					if (dst != lhs) // make INC/DEC not generate useless MOVs
						code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(unary2opcode(ir.ast), dst);
				} break;

				case IR::BINOP: {
					code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(binary2opcode(ir.ast), dst, rhs);
				} break;

				case IR::ARG_PUSH: {
					size_t addr = max_vars + max_tmps + dst;
					code.emplace_back(OP_MOV, addr, lhs);
				} break;
				case IR::ARG_POP: {
					
				} break;

				case IR::CALL: {
					auto* call = (AST_call*)ir.ast;

					istrs.push_back({ code.size(), format_source(ir.ast) });

					size_t args_addr = max_vars + max_tmps;

					if (call->fdef->type == A_FUNCDEF_BUILTIN) {
						auto* fdef = (AST_funcdef_builtin*)call->fdef;

						code.emplace_back(OP_CALLB, (size_t)(void*)fdef->func_ptr, args_addr);
					}
					else {
						auto* fdef = (AST_funcdef*)call->fdef;

						//code.emplace_back(OP_CALL);
					}
				} break;

				case IR::JUMP: {
					size_t lbl_id = ir.dst.id;
					istrs.push_back({ code.size(), format_jmp(ir.ast, lbl_id) });
					code.emplace_back(OP_JMP, lbl_id);
				} break;
				case IR::JUMP_CT: {
					size_t lbl_id = ir.dst.id;
					istrs.push_back({ code.size(), format_jmp(ir.ast, lbl_id) });
					code.emplace_back(OP_JNZ, lbl_id, lhs);
				} break;
				case IR::JUMP_CF: {
					size_t lbl_id = ir.dst.id;
					istrs.push_back({ code.size(), format_jmp(ir.ast, lbl_id) });
					code.emplace_back(OP_JZ, lbl_id, lhs);
				} break;

				case IR::RETURN: {
					// code for function epilogue
					code.emplace_back(OP_POP, stk_sz);
					code.emplace_back(OP_RET);
				} break;
			}
		}

		//{ // code for function epilogue
		//	code.emplace_back(OP_POP, stk_sz);
		//}

		// 3rd pass: patch up jump addresses
		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			switch (instr.code) {
				case OP_JMP:
				case OP_JNZ:
				case OP_JZ: {
					instr.dst = labels[instr.dst].addr;
				} break;
			}
		}
	}
};
