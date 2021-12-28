#pragma once
#include "basic_types.hpp"
#include "builtins.hpp"
#include "codegen.hpp"

struct VM {
	const size_t stack_size = 1024 * 64;
	int64_t* stack = new int64_t[stack_size];

	~VM () {
		delete stack;
	}

	void execute (Instruction* code, size_t code_sz, size_t entry_point) {
		printf("--------------------------------------------------------------------------------\n");
		printf("VM execute:\n");

		_execute(code, code_sz, stack, stack_size, entry_point);
	}
	static _NOINLINE void _execute (Instruction* code, size_t code_sz, int64_t* stack, size_t stack_size, size_t entry_point) {

		size_t program_counter = entry_point;
		size_t frame_ptr = 0;
		size_t stack_ptr = 0;

	#ifdef _DEBUG
		constexpr int DBGBYTE = 0xcd;
		memset(stack, DBGBYTE, sizeof(stack[0])*stack_size);
	#endif

		while (true) {
			assert(program_counter < code_sz);
			auto& op = code[program_counter++];

			size_t dst_val = op.dst;
			size_t src_val = op.src;

		#define SRC (assert(frame_ptr + src_val < stack_size), stack[frame_ptr + src_val])
		#define DST (assert(frame_ptr + dst_val < stack_size), stack[frame_ptr + dst_val])

		#define FSRC (assert(frame_ptr + src_val < stack_size), *(double*)&stack[frame_ptr + src_val])
		#define FDST (assert(frame_ptr + dst_val < stack_size), *(double*)&stack[frame_ptr + dst_val])

			switch (op.code) {
				case OP_NOP: {

				} continue;

				case OP_PUSH: {
					assert(stack_ptr + dst_val < stack_size);
					stack_ptr += dst_val;
				} continue;

				case OP_POP: {
					assert(stack_ptr >= dst_val);
					stack_ptr -= dst_val;
				#ifdef _DEBUG
					constexpr int DBGBYTE = 0xcd;
					memset(&stack[stack_ptr], DBGBYTE, sizeof(stack[0])*dst_val);
				#endif
				} continue;

				case OP_MOV | OP_IMM: {
					DST = src_val;
				} continue;
				case OP_MOV: {
					DST = SRC;
				} continue;

				case OP_CALLB: {
					auto* builtin_func = (builtin_func_t)dst_val;
					auto* arg_ptr = &stack[src_val];
					builtin_func((Value*)arg_ptr);
				} continue;

				case OP_JMP: {
					program_counter = dst_val;
				} continue;
				case OP_JNZ: {
					if (SRC != 0)
						program_counter = dst_val;
				} continue;
				case OP_JZ: {
					if (SRC == 0)
						program_counter = dst_val;
				} continue;

				case OP_NEG: {
					auto& i = DST;
					i = -i;
				} continue;
				case OP_NOT: {
					auto& i = DST;
					i = !i;
				} continue;

				case OP_INC              : DST++;                                      continue;
				case OP_DEC              : DST--;                                      continue;

				case OP_ADD              : DST += SRC;                                 continue;
				case OP_SUB              : DST -= SRC;                                 continue;
				case OP_MUL              : DST *= SRC;                                 continue;
				case OP_DIV              : DST /= SRC;                                 continue;
				case OP_REMAIND          : DST %= SRC;                                 continue;
				case OP_ADD     | OP_IMM : DST += (int64_t)src_val;                    continue;
				case OP_SUB     | OP_IMM : DST -= (int64_t)src_val;                    continue;
				case OP_MUL     | OP_IMM : DST *= (int64_t)src_val;                    continue;
				case OP_DIV     | OP_IMM : DST /= (int64_t)src_val;                    continue;
				case OP_REMAIND | OP_IMM : DST %= (int64_t)src_val;                    continue;

				case OP_LT               : DST = (int64_t)(DST <  SRC);                continue;
				case OP_LTE              : DST = (int64_t)(DST <= SRC);                continue;
				case OP_GT               : DST = (int64_t)(DST >  SRC);                continue;
				case OP_GTE              : DST = (int64_t)(DST >= SRC);                continue;
				case OP_EQ               : DST = (int64_t)(DST == SRC);                continue;
				case OP_NEQ              : DST = (int64_t)(DST != SRC);                continue;
				case OP_LT      | OP_IMM : DST = (int64_t)(DST <  (int64_t)src_val);   continue;
				case OP_LTE     | OP_IMM : DST = (int64_t)(DST <= (int64_t)src_val);   continue;
				case OP_GT      | OP_IMM : DST = (int64_t)(DST >  (int64_t)src_val);   continue;
				case OP_GTE     | OP_IMM : DST = (int64_t)(DST >= (int64_t)src_val);   continue;
				case OP_EQ      | OP_IMM : DST = (int64_t)(DST == (int64_t)src_val);   continue;
				case OP_NEQ     | OP_IMM : DST = (int64_t)(DST != (int64_t)src_val);   continue;

				case OP_FNEG: {
					auto& d = FDST;
					d = -d;
				} continue;

				case OP_FADD             : FDST += FSRC;                               continue;
				case OP_FSUB             : FDST -= FSRC;                               continue;
				case OP_FMUL             : FDST *= FSRC;                               continue;
				case OP_FDIV             : FDST /= FSRC;                               continue;
				case OP_FADD    | OP_IMM : FDST += *(double*)&op.src;                  continue;
				case OP_FSUB    | OP_IMM : FDST -= *(double*)&op.src;                  continue;
				case OP_FMUL    | OP_IMM : FDST *= *(double*)&op.src;                  continue;
				case OP_FDIV    | OP_IMM : FDST /= *(double*)&op.src;                  continue;

				case OP_FLT              : DST = (int64_t)(FDST <  FSRC);              continue;
				case OP_FLTE             : DST = (int64_t)(FDST <= FSRC);              continue;
				case OP_FGT              : DST = (int64_t)(FDST >  FSRC);              continue;
				case OP_FGTE             : DST = (int64_t)(FDST >= FSRC);              continue;
				case OP_FEQ              : DST = (int64_t)(FDST == FSRC);              continue;
				case OP_FNEQ             : DST = (int64_t)(FDST != FSRC);              continue;
				case OP_FLT     | OP_IMM : DST = (int64_t)(FDST <  *(double*)&op.src); continue;
				case OP_FLTE    | OP_IMM : DST = (int64_t)(FDST <= *(double*)&op.src); continue;
				case OP_FGT     | OP_IMM : DST = (int64_t)(FDST >  *(double*)&op.src); continue;
				case OP_FGTE    | OP_IMM : DST = (int64_t)(FDST >= *(double*)&op.src); continue;
				case OP_FEQ     | OP_IMM : DST = (int64_t)(FDST == *(double*)&op.src); continue;
				case OP_FNEQ    | OP_IMM : DST = (int64_t)(FDST != *(double*)&op.src); continue;
							   
				case OP_RET: {
					goto return_main;
				} continue;

				default:
					assert(false);
			}
		}

	return_main:
		assert(frame_ptr == 0);
		assert(stack_ptr == 0);
	}
};
