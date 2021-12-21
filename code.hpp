#pragma once
#include "common.hpp"

enum Opcode {
	OP_MOVI,     // MEM, IMM
	OP_MOV,      // MEM, MEM
	OP_PUSHI,    //      IMM
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
	OP_FEQ,      // MEM, MEM
	OP_FNEQ,     // MEM, MEM
};
inline constexpr const char* Opcode_str[] = {
	"MOVI", 
	"MOV",  
	"PUSHI",
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
	"FEQ",        
	"FNEQ",       
};

struct OpValue {
	int64_t val; // address or literal value
	AST* ast;

	void print () {
		switch (ast->type) {
			case A_LITERAL: {
				auto* lit = (AST_literal*)ast;
				switch (ast->valtype) {
					case BOOL: case INT:
						printf("%lli", lit->value.i);
						break;
					case FLT:
						printf("%f", lit->value.f);
						break;
					case STR:
						printf("%.*s", (int)(lit->a.source.end - lit->a.source.start), lit->a.source.start);
						break;
				}
			} break;

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* decl = (AST_vardecl*)var->decl;
				printf("%.*s [%lli]", (int)decl->ident.size(), decl->ident.data(), decl->stack_loc);
			} break;
			case A_VARDECL:
			case A_VARARGS: {
				auto* var = (AST_vardecl*)ast;
				//printf("%.*s [%s] (stk %lli)", (int)var->ident.size(), var->ident.data(), Type_str[var->a.valtype], var->stack_loc);
				printf("%.*s [%lli]", (int)var->ident.size(), var->ident.data(), var->stack_loc);
			} break;

			case A_FUNCDEF:
			case A_FUNCDEF_BUILTIN: {
				auto* fdef = (AST_funcdef*)ast;
				printf("%.*s", (int)fdef->decl.ident.size(), fdef->decl.ident.data());
			} break;
		}
	}
};

struct Instruction {
	Opcode  code;
	OpValue dst; // address of destination
	OpValue src; // address of source or immediate 64 bit value
};

void dbg_print (Instruction* code, size_t length) {

	printf("code:\n-------------------------------------------\n");
	for (size_t i=0; i<length; ++i) {
		auto& op = code[i];
		
		bool dst, src;

		switch (op.code) {
			case OP_MOVI: case OP_MOV:
			case OP_CALL: case OP_CALLB:
			case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: case OP_REMAIND:
			case OP_LT: case OP_LTE: case OP_EQ: case OP_NEQ:
			case OP_FADD: case OP_FSUB: case OP_FMUL: case OP_FDIV:
			case OP_FLT: case OP_FLTE: case OP_FEQ: case OP_FNEQ:
			case OP_JMP: case OP_JNZ: case OP_JZ:
				dst = true;
				src = true;
				break;
			case OP_INC: case OP_DEC:
			case OP_NEG: case OP_NOT: case OP_FNEG:
				dst = true;
				src = false;
				break;
			case OP_PUSHI: case OP_PUSH:
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

		if (dst) printf("%16lli", op.dst.val);
		else     printf("%16s", "");

		if (src) printf("%16lli", op.src.val);
		else     printf("%16s", "");

		printf("   # ");

		if ((dst || op.code == OP_POP) && op.dst.ast) {
			op.dst.print();

			if (src)
				printf(", ");
		}

		if (src && op.src.ast) {
			op.src.print();
		}
		printf("\n");
	}
	printf("-------------------------------------------\n");
}
