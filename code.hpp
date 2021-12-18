#pragma once
#include "common.hpp"

enum Opcode {
	OP_MOVI,  // MEM, IMM
	OP_MOV,   // MEM, MEM
	OP_PUSHI, //      IMM
	OP_PUSH,  //      MEM
	OP_POP,   //         

	// call bytecode function
	OP_CALL,
	// call builtin (native) function
	OP_CALLB,
};
inline constexpr const char* Opcode_str[] = {
	"MOVI", 
	"MOV",  
	"PUSHI",
	"PUSH", 
	"POP", 
	"CALL",
	"CALLB",
};

struct IRValue {
	size_t val; // address or literal value
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

struct IR_Op {
	Opcode op;
	IRValue dst; // address of destination
	IRValue src; // address of source or immediate 64 bit value
};

void dbg_print (IR_Op* code, size_t length) {

	printf("code:\n-------------------------------------------\n");
	for (size_t i=0; i<length; ++i) {
		auto& op = code[i];
		
		bool dst = true, src = true;

		switch (op.op) {
			case OP_MOVI: case OP_MOV:
				dst = true;
				break;
			case OP_PUSHI: case OP_PUSH:
				dst = false;
				break;
			case OP_CALL: case OP_CALLB:
				src = false;
				break;
			case OP_POP:
				dst = false;
				src = false;
				break;
		}

		printf("%-6s", Opcode_str[op.op]);

		if (dst) printf("%16lli", op.dst.val);
		else     printf("%16s", "");

		if (src) printf("%16lli", op.src.val);
		else     printf("%16s", "");

		printf("   # ");

		if (dst || op.op == OP_POP) {
			op.dst.print();

			if (src)
				printf(", ");
		}

		if (src) {
			op.src.print();
		}
		printf("\n");
	}
	printf("-------------------------------------------\n");
}
