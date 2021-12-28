#pragma once
#include "common.hpp"
#include "ir_gen.hpp"

enum Opcode : uint32_t {
	OP_IMM = 1u << 6,

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

	_OP_COUNT,
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

static_assert(_OP_COUNT < OP_IMM, "");

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

	std::vector<Info> istrs;

	std::vector<size_t> vars;
	std::vector<size_t> temps;

	void dbg_print (IR::IR const& ir) {
		printf("--------------------------------------------------------------------------------\n");
		printf("bytecode:\n");

		auto cur_lbl_id = labels_ordered.begin();
		auto cur_istr = istrs.begin();

		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			while (cur_lbl_id < labels_ordered.end() && ir.labels[*cur_lbl_id].codeaddr == i) {
				printf("       %s:\n", ir.labels[*cur_lbl_id].name);
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
				printf(" %18s", prints("[%llx]", val).c_str());
			};
			auto print_imm = [] (size_t val) {
				printf(" %18llx", val);
			};

			printf("  %-5s", Opcode_str[code]);
			
			if (jmp) {
				print_imm(instr.dst);
			}
			else {
				if (dst)        print_operand(instr.dst);
				else            printf(" %18s", "");
			}

			if (src && src_imm) print_imm(instr.src);
			else if (src)       print_operand(instr.src);
			else                printf(" %18s", "");

			printf("  # ");

			while (cur_istr < istrs.end() && cur_istr->addr == i) {
				printf("%s", cur_istr->str);
				cur_istr++;
			}

			printf("\n");
		}
	}

	void generate (IR::IR& ir) {

		code .reserve(1024 * 8);
		istrs.reserve(1024 * 8);
		
		auto get_addr = [&] (IR::Var var) {
			size_t addr = var.id;
			if (var.type == IR::TEMP) addr += ir.var_count;
			return addr;
		};
		auto format_const = [] (AST* ast) {
			std::string_view text;
			if (ast->type == A_ASSIGN) {
				auto* op = (AST_binop*)ast;

				text = op->rhs->src_tok->source.text();
			}
			else {
				assert(ast->type == A_LITERAL);
				text = ast->src_tok->source.text();
			}
			return format("=%.*s", (int)text.size(), text.data());
		};
		auto format_source = [] (AST* ast) {
			auto text = ast->src_tok->source.text();
			return format("%.*s", (int)text.size(), text.data());
		};
		auto format_jmp = [&] (AST* ast, size_t lbl_id) {
			auto text = ast->src_tok->source.text();
			return format("%s, %.*s", ir.labels[lbl_id].name, (int)text.size(), text.data());
		};

		size_t args_addr = ir.var_count + ir.temp_count;
		size_t stk_sz    = ir.var_count + ir.temp_count + ir.max_retargs;

		// code for function prologe
		code.emplace_back(OP_PUSH, stk_sz);

		auto imm = [] (Opcode op, IR::Var rhs) {
			if (rhs.type == IR::T_CONST)
				op |= OP_IMM;
			return op;
		};

		// generate code
		for (size_t i=0; i<ir.code.size(); ++i) {
			auto& iri = ir.code[i];

			size_t dst = get_addr(iri.dst);
			size_t lhs = get_addr(iri.lhs);
			size_t rhs = get_addr(iri.rhs);
			switch (iri.type) {
				case IR::LABEL: {
					assert(iri.dst.type == IR::T_LABEL);
				} break;

				case IR::MOVE: {
					assert(iri.dst.type == IR::VAR || iri.dst.type == IR::TEMP);
					assert(iri.lhs.type == IR::VAR || iri.lhs.type == IR::TEMP || iri.lhs.type == IR::T_CONST);
				} break;

				case IR::UNOP: {
					assert(iri.dst.type == IR::VAR || iri.dst.type == IR::TEMP);
					assert(iri.lhs.type == IR::VAR || iri.lhs.type == IR::TEMP); // unary op arg should not be a constant, since const folding should remove the unary op then
				} break;

				case IR::BINOP: {
					assert(iri.dst.type == IR::VAR || iri.dst.type == IR::TEMP);
					assert(iri.lhs.type == IR::VAR || iri.lhs.type == IR::TEMP);
					assert(iri.rhs.type == IR::VAR || iri.rhs.type == IR::TEMP || iri.rhs.type == IR::T_CONST);
				} break;

				case IR::ARG_PUSH: {
					assert(iri.dst.type == IR::T_ARG);
					assert(iri.lhs.type == IR::VAR || iri.lhs.type == IR::TEMP || iri.lhs.type == IR::T_CONST);
					assert(iri.rhs.type == IR::UNDEFINED);
				} break;

				case IR::JUMP: {
					assert(iri.dst.type == IR::T_LABEL);
					assert(iri.lhs.type == IR::UNDEFINED);
				} break;
				case IR::JUMP_CT:
				case IR::JUMP_CF: {
					assert(iri.dst.type == IR::T_LABEL);
					assert(iri.lhs.type == IR::VAR || iri.lhs.type == IR::TEMP); // cond can't be CONSTVAR since that should be turned into a unconditional jmp
				} break;
			}

			switch (iri.type) {
				case IR::LABEL: {
					size_t lbl_id = iri.dst.id;
					ir.labels[lbl_id].codeaddr = code.size();

					labels_ordered.push_back(lbl_id);
				} break;

				case IR::MOVE: {
					//if (ir.ast && ir.ast->type == A_ASSIGN) {
					//	auto* op = (AST_binop*)ir.ast;
					//	auto* alhs = op->lhs;
					//	istrs.push_back({ code.size(), format_source(alhs) });
					//}

					if (iri.lhs.type == IR::T_CONST)
						istrs.push_back({ code.size(), format_const(iri.ast) });
					
					code.emplace_back(imm(OP_MOV, iri.lhs), dst, lhs);
				} break;

				case IR::UNOP: {
					if (dst != lhs) // make INC/DEC not generate useless MOVs
						code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(unary2opcode(iri.ast), dst);
				} break;

				case IR::BINOP: {
					if (dst != lhs)
						code.emplace_back(OP_MOV, dst, lhs);
					code.emplace_back(imm(binary2opcode(iri.ast), iri.rhs), dst, rhs);
				} break;

				case IR::RET_PUSH: {
					
				} break;
				case IR::RET_POP: {
					size_t addr = args_addr + lhs;
					code.emplace_back(OP_MOV, dst, addr);
				} break;

				case IR::ARG_PUSH: {
					size_t addr = args_addr + dst;
					code.emplace_back(imm(OP_MOV, iri.lhs), addr, lhs);
				} break;
				case IR::ARG_POP: {
					
				} break;

				case IR::CALL: {
					auto* call = (AST_call*)iri.ast;

					istrs.push_back({ code.size(), format_source(iri.ast) });

					if (call->fdef->type == A_FUNCDEF_BUILTIN) {
						auto* fdef = (AST_funcdef_builtin*)call->fdef;

						code.emplace_back(OP_CALLB, (size_t)(void*)fdef->func_ptr, args_addr);
					}
					else {
						auto* fdef = (AST_funcdef*)call->fdef;

						//code.emplace_back(OP_CALL);
					}
				} break;

				case IR::JUMP:
				case IR::JUMP_CT:
				case IR::JUMP_CF: {
					Opcode op;
					switch (iri.type) {
						case IR::JUMP:    op = OP_JMP; break;
						case IR::JUMP_CT: op = OP_JNZ; break;
						case IR::JUMP_CF: op = OP_JZ;  break;
					}

					size_t lbl_id = iri.dst.id;
					istrs.push_back({ code.size(), ir.labels[lbl_id].name });
					code.emplace_back(op, lbl_id, lhs);
				} break;

				case IR::RETURN: {
					// code for function epilogue
					code.emplace_back(OP_POP, stk_sz);
					code.emplace_back(OP_RET);
				} break;

				case IR::VARDECL:
				case IR::DEAD: {
					// do nothing
				} break;

				default:
					assert(false);
			}
		}

		//{ // code for function epilogue
		//	code.emplace_back(OP_POP, stk_sz);
		//}

		// patch up jump addresses
		for (size_t iaddr=0; iaddr<code.size(); ++iaddr) {
			auto& instr = code[iaddr];

			switch (instr.code) {
				case OP_JMP:
				case OP_JNZ:
				case OP_JZ: {
					instr.dst = ir.labels[instr.dst].codeaddr;
				} break;
			}
		}
	}
};
