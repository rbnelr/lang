#pragma once
#include "common.hpp"
#include "ir_gen.hpp"

/*  calling convention:
	A() calls B() calls C():

	A::local vars   <-  A frame ptr
	A::local temps

	...
	B::arg1
	B::arg0
	...
	B::ret1
	B::ret0         <-  A stack ptr

	A frame ptr
	A return address

	B::local vars   <-  B frame ptr
	B::local temps

	...
	C::arg1
	C::arg0
	...
	C::ret1
	C::ret0         <-  B stack ptr

	B frame ptr
	B return address

	C::local vars   <-  C frame ptr
	C::local temps
*/

enum Opcode : uint32_t {
	OPC_IMM = 1u << 6,

	OPC_NOP=0,

	OPC_MOV,   

	OPC_SPUSH, 
	OPC_SPOP,  

	OPC_PUSH,  
	OPC_POP,   

	// call bytecode function
	OPC_CALL,
	// call builtin (native) function
	OPC_CALLB,

	OPC_RET,

	OPC_JMP,     // JUMP DST
	OPC_JNZ,     // JUMP DST, COND     jump if nonzero  
	OPC_JZ,      // JUMP DST, COND     jump if zero     

	OPC_NEG,    
	OPC_NOT,    
	OPC_INC,    
	OPC_DEC,    

	OPC_ADD,	   
	OPC_SUB,	   
	OPC_MUL,	   
	OPC_DIV,	   
	OPC_REMAIND,
	OPC_LT,     
	OPC_LE,    
	OPC_GT,     
	OPC_GE,    
	OPC_EQ,     
	OPC_NEQ,    

	OPC_FNEG,

	OPC_FADD, 
	OPC_FSUB, 
	OPC_FMUL, 
	OPC_FDIV, 
	_OPC_FREMAIND, // dummy
	OPC_FLT,  
	OPC_FLE, 
	OPC_FGT,  
	OPC_FGE, 
	OPC_FEQ,  
	OPC_FNEQ, 

	_OPC_COUNT,
};
inline constexpr const char* Opcode_str[] = {
	"NOP",  

	"MOV",  

	"SPUSH", 
	"SPOP", 

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

static_assert(_OPC_COUNT < OPC_IMM, "");

ENUM_BITFLAG_OPERATORS_TYPE(Opcode, uint32_t)

struct Instruction {
	Opcode   code;
	intptr_t dst; // address of destination
	intptr_t src; // address of source or immediate 64 bit value

	Instruction (Opcode code, int64_t dst=0, int64_t src=0): code{code}, dst{dst}, src{src} {}
};

Opcode ir2opcode (IR::IROpType op) {
	using namespace IR;
	switch (op) {
		case OP_b_EQ       : return OPC_EQ ;
		case OP_b_NEQ      : return OPC_NEQ;
		case OP_b_NOT      : return OPC_NOT;
		
		case OP_i_ADD      : return OPC_ADD    ;
		case OP_i_SUB      : return OPC_SUB    ;
		case OP_i_MUL      : return OPC_MUL    ;
		case OP_i_DIV      : return OPC_DIV    ;
		case OP_i_REMAIND  : return OPC_REMAIND;
		
		case OP_i_LT       : return OPC_LT ;
		case OP_i_LE       : return OPC_LE ;
		case OP_i_GT       : return OPC_GT ;
		case OP_i_GE       : return OPC_GE ;
		case OP_i_EQ       : return OPC_EQ ;
		case OP_i_NEQ      : return OPC_NEQ;
		
		case OP_i_NEG      : return OPC_NEG;
		case OP_i_NOT      : return OPC_NOT;
		case OP_i_INC      : return OPC_INC;
		case OP_i_DEC      : return OPC_DEC;
		
		case OP_f_ADD      : return OPC_FADD;
		case OP_f_SUB      : return OPC_FSUB;
		case OP_f_MUL      : return OPC_FMUL;
		case OP_f_DIV      : return OPC_FDIV;
		
		case OP_f_LT       : return OPC_FLT ;
		case OP_f_LE       : return OPC_FLE ;
		case OP_f_GT       : return OPC_FGT ;
		case OP_f_GE       : return OPC_FGE ;
		case OP_f_EQ       : return OPC_FEQ ;
		case OP_f_NEQ      : return OPC_FNEQ;
		
		case OP_f_NEG      : return OPC_FNEG;

		INVALID_DEFAULT;
	}
}

struct Codegen {
	std::vector<Instruction> code;

	struct Info {
		size_t       addr;
		const char*  str;
	};

	std::vector<Info> code_labels; // lbl ids in order of code addr

	struct InstrRange {
		size_t first, end;
	};
	std::vector<InstrRange> func_code;

	std::vector<Info> istrs;

	std::vector<size_t> vars;
	std::vector<size_t> temps;

	void dbg_print () {
		printf("--------------------------------------------------------------------------------\n");
		printf("bytecode:\n");

		auto cur_lbli = code_labels.begin();
		auto cur_istr = istrs.begin();

		for (size_t i=0; i<code.size(); ++i) {
			auto& instr = code[i];

			while (cur_lbli < code_labels.end() && cur_lbli->addr == i) {
				// detect function label and print it a little more highlighted
				if (memcmp(cur_lbli->str, "func", 4) == 0) {
					printf("\n%s:\n", cur_lbli->str);
				}
				else {
					printf("       %s:\n", cur_lbli->str);
				}
				cur_lbli++;
			}

			bool dst=true, src=true;

			Opcode code  = instr.code & ~OPC_IMM;

			bool imm = (instr.code & OPC_IMM) != 0;

			bool src_imm = false;
			bool dst_imm = false;

			switch (code) {
				case OPC_RET:
					dst = false;
					src = false;
					break;

				case OPC_SPUSH:
				case OPC_SPOP:
					dst_imm = true;
					src = false;
					break;

				case OPC_PUSH:
				case OPC_POP:
					dst = false;
					break;

				case OPC_JMP:
					dst_imm = true;
					src     = false;
					break;
				case OPC_JNZ:
				case OPC_JZ:
					dst_imm = true;
					break;

				case OPC_NEG:
				case OPC_NOT:
				case OPC_INC:
				case OPC_DEC:
				case OPC_FNEG:
					src = false;
					break;

				case OPC_CALL:
				case OPC_CALLB:
					dst_imm = true;
					src = false;
					break;
			}

			if (imm) {
				if (!src) dst_imm = true;
				else      src_imm = true;
			}

			printf("%5llx |", i);

			auto print_operand = [] (intptr_t val) {
				printf(" %18s", prints(val >= 0 ? "[%llx]" : "[-%llx]", abs(val)).c_str());
			};
			auto print_imm = [] (intptr_t val) {
				printf(" %18llx", val);
			};

			printf("  %-7s", prints("%s%s", Opcode_str[code], imm ? "i":"").c_str());
			
			if (dst && dst_imm) print_imm(instr.dst);
			else if (dst)       print_operand(instr.dst);
			else                printf(" %18s", "");

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

	void generate (IR::IRGen& ir) {

		code .reserve(1024 * 8);
		istrs.reserve(1024 * 8);

		func_code.resize(ir.funcdefs.size());

		for (size_t funcid=0; funcid<ir.funcdefs.size(); ++funcid) {
			auto& funcdef = ir.funcdefs[funcid];
			auto& fir     = ir.func_irs[funcid];

			func_code[funcid].first = code.size();

			auto func_lbl_name = format("func %.*s()", (int)funcdef->ident.size(), funcdef->ident.data());
			code_labels.push_back({ code.size(), func_lbl_name });

			add_func_code(fir);

			func_code[funcid].end = code.size();
		}

		// patch up jump addresses
		for (size_t funcid=0; funcid<ir.funcdefs.size(); ++funcid) {
			auto& func_ir = ir.func_irs[funcid];

			for (size_t iaddr = func_code[funcid].first; iaddr < func_code[funcid].end; ++iaddr) {
				auto& instr = code[iaddr];

				switch (instr.code) {

					case OPC_JMP:
					case OPC_JNZ:
					case OPC_JZ: {
						size_t func_lbl_id = instr.dst;
						size_t code_lbl_id = func_ir.labels[func_lbl_id].code_lbl_id;
						instr.dst = code_labels[code_lbl_id].addr;
					} break;

					case OPC_CALL: {
						size_t func_id = instr.dst;
						instr.dst = func_code[func_id].first;
					} break;
				}
			}
		}
	}

	const char* format_const (AST* ast) {
		std::string_view text;
		if (ast->type == A_ASSIGNOP && ((AST_binop*)ast)->op == OP_ASSIGN) {
			auto* op = (AST_binop*)ast;

			text = op->rhs->src_tok->source.text();
		}
		else if (ast->type == A_VARDECL) {
			// ???
		}
		else {
			assert(ast->type == A_LITERAL);
			text = ast->src_tok->source.text();
		}
		return format("=%.*s", (int)text.size(), text.data());
	}
	const char* format_source (AST* ast) {
		auto text = ast->src_tok->source.text();
		return format("%.*s", (int)text.size(), text.data());
	}
	
	void add_func_code (IR::IR& ir) {
		std::vector<size_t> temp_locs;

		temp_locs.resize(ir.temp_count);

		size_t temps_count = 0;

		for (size_t i=0; i<ir.code.size(); ++i) {
			auto& iri = ir.code[i];

			switch (iri.type) {
				case IR::MOVE:
				case IR::UNOP:
				case IR::BINOP: {
					if (iri.dst.type == IR::VT_TEMPID)
						temp_locs[iri.dst.id] = temps_count++;
				} break;
			}
		}

		//// generate code
		
		size_t stk_sz = ir.var_count + temps_count + ir.max_retargs;

		size_t retarg_base = stk_sz-1;

		auto get_addr = [&] (IR::Var var) -> intptr_t {
			if (var.type == IR::VT_CONST)
				return (intptr_t)var.id;

			if (var.type == IR::VT_ARGID)
				return -1-2 - (intptr_t)var.id;
			if (var.type == IR::VT_TEMPID)
				return (intptr_t)temp_locs[var.id] + ir.var_count; // temps are after vars
			return (intptr_t)var.id;
		};

		auto imm = [] (Opcode op, IR::Var rhs) {
			if (rhs.type == IR::VT_CONST)
				op |= OPC_IMM;
			return op;
		};

		auto nonimm = [] (IR::VarType type) {
			return type == IR::VT_VARID || type == IR::VT_TEMPID || type == IR::VT_ARGID;
		};
		auto maybeimm = [] (IR::VarType type) {
			return type == IR::VT_VARID || type == IR::VT_TEMPID || type == IR::VT_ARGID || type == IR::VT_CONST;
		};

		// code for function prologe
		if (stk_sz > 0)
			code.emplace_back(OPC_SPUSH, stk_sz);

		size_t retargid = 0;

		for (size_t i=0; i<ir.code.size(); ++i) {
			auto& iri = ir.code[i];

			intptr_t dst = get_addr(iri.dst);
			intptr_t lhs = get_addr(iri.lhs);
			intptr_t rhs = get_addr(iri.rhs);
			
			switch (iri.type) {
				case IR::LABEL: {
					assert(iri.dst.type == IR::VT_LABELID);
				} break;

				case IR::MOVE: {
					assert(nonimm(iri.dst.type));
					assert(maybeimm(iri.lhs.type));
				} break;

				case IR::UNOP: {
					assert(nonimm(iri.dst.type));
					assert(maybeimm(iri.lhs.type));
				} break;

				case IR::BINOP: {
					assert(nonimm(iri.dst.type));
					assert(maybeimm(iri.lhs.type));
					assert(maybeimm(iri.rhs.type));
				} break;

				case IR::JUMP: {
					assert(iri.dst.type == IR::VT_LABELID);
					assert(iri.lhs.type == IR::VT_UNDEFINED);
				} break;
				case IR::JUMP_CT:
				case IR::JUMP_CF: {
					assert(iri.dst.type == IR::VT_LABELID);
					assert(nonimm(iri.lhs.type)); // cond can't be CONSTVAR since that should be turned into a unconditional jmp
				} break;
			}

			switch (iri.type) {
				case IR::LABEL: {
					size_t lbl_id = iri.dst.id;
					ir.labels[lbl_id].code_lbl_id = code_labels.size();
					code_labels.push_back({ code.size(), ir.labels[lbl_id].name });
				} break;

				case IR::MOVE: {
					//if (ir.ast && ir.ast->type == A_ASSIGN) {
					//	auto* op = (AST_binop*)ir.ast;
					//	auto* alhs = op->lhs;
					//	istrs.push_back({ code.size(), format_source(alhs) });
					//}

					if (iri.lhs.type == IR::VT_CONST)
						istrs.push_back({ code.size(), format_const(iri.ast) });

					code.emplace_back(imm(OPC_MOV, iri.lhs), dst, lhs);
				} break;

				case IR::UNOP: {
					auto opc = ir2opcode(iri.op);

					if (dst != lhs) // make INC/DEC not generate useless MOVs
						code.emplace_back(imm(OPC_MOV, iri.lhs), dst, lhs);
					code.emplace_back(opc, dst);
				} break;

				case IR::BINOP: {
					auto opc = ir2opcode(iri.op);

					if (dst != lhs)
						code.emplace_back(imm(OPC_MOV, iri.lhs), dst, lhs);
					code.emplace_back(imm(opc, iri.rhs), dst, rhs);
				} break;

				case IR::PUSH_RET: {
					retargid++;
				} break;
				case IR::PUSH_ARG: {
					intptr_t addr = retarg_base - retargid;
					code.emplace_back(imm(OPC_MOV, iri.lhs), addr, lhs);

					retargid++;
				} break;

				case IR::CALL: {
					auto* call = (AST_call*)iri.ast;
					auto* fdef = (AST_funcdef*)call->fdef;

					istrs.push_back({ code.size(), format_source(iri.ast) });

					if (call->fdef->type == A_FUNCDEF_BUILTIN) {
						auto* fdef = (AST_funcdef_builtin*)call->fdef;
						code.emplace_back(OPC_CALLB, (size_t)(void*)fdef->func_ptr);
					}
					else {
						auto* fdef = (AST_funcdef*)call->fdef;
						code.emplace_back(OPC_CALL, fdef->codegen_funcid);
					}

					retargid = 0;
				} break;

				case IR::GET_RET: {
					intptr_t addr = retarg_base - iri.lhs.id;
					code.emplace_back(OPC_MOV, dst, addr);
				} break;

				case IR::JUMP:
				case IR::JUMP_CT:
				case IR::JUMP_CF: {
					Opcode op;
					switch (iri.type) {
						case IR::JUMP:    op = OPC_JMP; break;
						case IR::JUMP_CT: op = OPC_JNZ; break;
						case IR::JUMP_CF: op = OPC_JZ;  break;
					}

					size_t lbl_id = iri.dst.id;
					istrs.push_back({ code.size(), ir.labels[lbl_id].name });
					code.emplace_back(op, lbl_id, lhs);
				} break;

				case IR::RETURN: {
					// code for function epilogue
					if (stk_sz > 0)
						code.emplace_back(OPC_SPOP, stk_sz);
					code.emplace_back(OPC_RET);
				} break;

				case IR::DEAD: {
					// do nothing
				} break;

				default:
					assert(false);
			}
		}

		//{ // code for function epilogue
		//	code.emplace_back(OPC_POP, stk_sz);
		//}
	}
};
