#pragma once
#include "common.hpp"
#include "ir_gen.hpp"

enum Opcode : uint32_t {
	OP_IMM = 1u << 31,

	OP_MOV=0,      // MEM, MEM
	OP_PUSH,     //      MEM
	OP_POP,      //         

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

void dbg_print (Instruction* code, size_t length) {

	printf("code:\n-------------------------------------------\n");
	for (size_t i=0; i<length; ++i) {
		auto& op = code[i];
		
		bool dst, src;

		switch (op.code) {
			case OP_MOV:
			case OP_CALL: case OP_CALLB:
			case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: case OP_REMAIND:
			case OP_LT: case OP_LTE: case OP_GT: case OP_GTE: case OP_EQ: case OP_NEQ:
			case OP_FADD: case OP_FSUB: case OP_FMUL: case OP_FDIV:
			case OP_FLT: case OP_FLTE: case OP_FGT: case OP_FGTE: case OP_FEQ: case OP_FNEQ:
			case OP_JMP: case OP_JNZ: case OP_JZ:
				dst = true;
				src = true;
				break;
			case OP_INC: case OP_DEC:
			case OP_NEG: case OP_NOT: case OP_FNEG:
				dst = true;
				src = false;
				break;
			case OP_PUSH:
				dst = false;
				src = true;
				break;
			case OP_POP:
			case OP_RET:
				dst = false;
				src = false;
				break;
			default:
				assert(false);
		}

		printf("%5lli | %-6s", i, Opcode_str[op.code]);

		if (dst) printf("%16lli", op.dst);
		else     printf("%16s", "");

		if (src) printf("%16lli", op.src);
		else     printf("%16s", "");

		printf("\n");
	}
	printf("-------------------------------------------\n");
}

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
				case A_ADD:       case A_ADDEQ:       return OP_ADD;
				case A_SUB:       case A_SUBEQ:       return OP_SUB;
				case A_MUL:       case A_MULEQ:       return OP_MUL;
				case A_DIV:       case A_DIVEQ:       return OP_DIV;
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
				case A_ADD:       case A_ADDEQ:       return OP_FADD;
				case A_SUB:       case A_SUBEQ:       return OP_FSUB;
				case A_MUL:       case A_MULEQ:       return OP_FMUL;
				case A_DIV:       case A_DIVEQ:       return OP_FDIV;
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

	std::vector<Info> labels;
	std::vector<size_t> label_id2idx;

	//std::vector<Info> instrucs;

	std::vector<size_t> vars;
	std::vector<size_t> temps;

	template <typename T>
	void grow (std::vector<T>& vec, size_t min_sz) {
		if (vec.size() < min_sz)
			vec.resize(min_sz);
	}

	void dbg_print () {
		printf("bytecode : --------------\n");

		auto cur_label = labels.begin();

		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			while (cur_label < labels.end() && cur_label->addr == i) {
				printf("       %s:\n", cur_label->str);
				cur_label++;
			}

			bool dst=true, src=true;

			Opcode code  = instr.code & ~OP_IMM;
			bool src_imm = (instr.code & OP_IMM) != 0;

			switch (code) {
				case OP_RET:
					dst = false;
					src = false;
					break;

				case OP_JMP:
				case OP_JNZ:
				case OP_JZ:
				case OP_NEG:
				case OP_NOT:
				case OP_INC:
				case OP_DEC:
				case OP_FNEG:
					src = false;
					break;
			}

			printf("%5lli |", i);

			auto print_operand = [] (size_t val) {
				printf(" %16s", prints("[%llx]", val).c_str());
			};
			auto print_imm = [] (size_t val) {
				printf(" %16llx", val);
			};

			printf("  %-5s", Opcode_str[code]);

			if (dst)            print_operand(instr.dst);
			else                printf(" %16s", "");

			if (src && src_imm) print_imm(instr.src);
			else if (src)       print_operand(instr.src);
			else                printf(" %16s", "");

			printf("  # ");

			printf("\n");
		}
	}

	void generate (std::vector<IR::Instruction>& ir_code) {
		
		size_t max_vars = 0;
		size_t max_tmps = 0;

		for (size_t i=0; i<ir_code.size(); ++i) {
			auto& ir = ir_code[i];

			switch (ir.type) {
				case IR::LABEL: {
					size_t lbl_id = ir.dst.id;

					auto& lbl = labels.emplace_back(Info{ 0, format("%s(%lli)", ir.name, ir.dst.id) });
					size_t idx = &lbl - labels.data();

					grow(label_id2idx, lbl_id+1);
					label_id2idx[lbl_id] = idx;
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
			}
		}

		auto get_addr = [&] (IR_Var var) {
			size_t addr = var.id;
			if (var.is_temp) addr += max_vars;
			return addr;
		};

		for (size_t i=0; i<ir_code.size(); ++i) {
			auto& ir = ir_code[i];

			size_t dst = get_addr(ir.dst);
			size_t lhs = get_addr(ir.lhs);
			size_t rhs = get_addr(ir.rhs);

			switch (ir.type) {
				case IR::LABEL: {
					size_t lbl_id = ir.dst.id;
					labels[label_id2idx[lbl_id]].addr = code.size();
				} break;

				case IR::STK_PUSH:
				case IR::STK_POP: {
					// no-op
				} break;

				case IR::MOVE: {
					code.emplace_back(OP_MOV, dst, lhs);
				} break;

				case IR::CONST: {
					code.emplace_back(OP_MOV | OP_IMM, dst, ir.const_val);
				} break;

				case IR::UNOP: {
					code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(unary2opcode(ir.ast), dst);
				} break;

				case IR::BINOP: {
					code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(binary2opcode(ir.ast), dst, rhs);
				} break;

				case IR::CALL: {
					code.emplace_back(OP_CALL);
				} break;

				case IR::RETURN: {
					code.emplace_back(OP_RET);
				} break;
			}
		}

		for (size_t i=0; i<ir_code.size(); ++i) {
			auto& ir = ir_code[i];

			size_t dst = get_addr(ir.dst);
			size_t lhs = get_addr(ir.lhs);
			size_t rhs = get_addr(ir.rhs);

			switch (ir.type) {
				case IR::LABEL: {
					size_t lbl_id = ir.dst.id;
					labels[label_id2idx[lbl_id]].addr = code.size();
				} break;

				case IR::STK_PUSH:
				case IR::STK_POP: {
					// no-op
				} break;

				case IR::MOVE: {
					code.emplace_back(OP_MOV, dst, lhs);
				} break;

				case IR::CONST: {
					code.emplace_back(OP_MOV | OP_IMM, dst, ir.const_val);
				} break;

				case IR::UNOP: {
					code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(unary2opcode(ir.ast), dst);
				} break;

				case IR::BINOP: {
					code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(binary2opcode(ir.ast), dst, rhs);
				} break;

				case IR::CALL: {
					code.emplace_back(OP_CALL);
				} break;

				case IR::RETURN: {
					code.emplace_back(OP_RET);
				} break;
			}
		}
	}

	/*
	LABEL,    // used to mark places jumps go to

	STK_PUSH, // Create a new scope
	STK_POP,  // Close the last scope, making stack space be reused (even without optimizations)

	VARDECL,  // dst

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
	*/
};
