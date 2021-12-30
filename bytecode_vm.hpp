#pragma once
#include "basic_types.hpp"
#include "builtins.hpp"
#include "codegen.hpp"

_NOINLINE void _execute (Instruction* code, size_t code_sz, intptr_t* stack, size_t stack_size, size_t entry_point) {

	// Use ugly pointers instead of simple indices since this is faster to execute due to simpler address calculation

	Instruction* pentry_point = &code[entry_point];
	Instruction* program_counter = pentry_point;
	intptr_t*    frame_ptr = stack;
	intptr_t*    stack_ptr = stack;

	Instruction* code_end  = code + code_sz;
	intptr_t*    stack_end = stack + stack_size;

#define DBG_UNINIT 0xcc
#define DBG_FREED  0xcd

#ifdef _DEBUG
#define STK_CLEAR(N, VAL) memset(stack_ptr, VAL, sizeof(stack[0])*(N))
#else
#define STK_CLEAR(N, VAL) 
#endif

	auto PUSH = [&] (int64_t val) {
		assert(stack_ptr < stack_end);
		*stack_ptr++ = val;
	};
	auto POP = [&] () {
		assert(stack_ptr > stack);
		--stack_ptr;
		auto val = *stack_ptr;
		STK_CLEAR(1, DBG_FREED);
		return val;
	};

	auto PUSHN = [&] (int64_t N) {
		assert(stack_ptr + N <= stack_end);
		STK_CLEAR(N, DBG_UNINIT);
		stack_ptr += N;
	};
	auto POPN  = [&] (int64_t N) {
		assert(stack_ptr >= stack + N);
		stack_ptr -= N;
		STK_CLEAR(N, DBG_FREED);
	};

	STK_CLEAR(stack_size, DBG_FREED);
	
	PUSH((intptr_t)frame_ptr);
	PUSH(0); // 0 return address signals return from main

	frame_ptr = stack_ptr;

	while (true) {
		assert(program_counter < code_end);
		auto& op = *program_counter++;

		size_t dst_val = op.dst;
		size_t src_val = op.src;

	#define SRC (assert(frame_ptr + src_val < stack_end), frame_ptr[src_val])
	#define DST (assert(frame_ptr + dst_val < stack_end), frame_ptr[dst_val])

	#define FSRC (assert(frame_ptr + src_val < stack_end), *(double*)(frame_ptr + src_val))
	#define FDST (assert(frame_ptr + dst_val < stack_end), *(double*)(frame_ptr + dst_val))

		switch (op.code) {
			case OP_NOP: {

			} continue;

			case OP_SPUSH: {
				PUSHN(dst_val);
			} continue;
			case OP_SPOP: {
				POPN(dst_val);
			} continue;

			case OP_PUSH: {
				PUSH(SRC);
			} continue;
			case OP_PUSH | OP_IMM: {
				PUSH(src_val);
			} continue;
			case OP_POP: {
				DST = POP();
			} continue;

			case OP_MOV: {
				DST = SRC;
			} continue;
			case OP_MOV | OP_IMM: {
				DST = src_val;
			} continue;

			case OP_CALLB: {
				auto* builtin_func = (builtin_func_t)dst_val;
				auto* argptr = (Value*)(stack_ptr-1);

				builtin_func(argptr);
			} continue;

			case OP_CALL: {
				PUSH((intptr_t)frame_ptr);
				PUSH((intptr_t)program_counter);

				frame_ptr       = stack_ptr;
				program_counter = dst_val + code;
			} continue;

			case OP_RET: {
				program_counter = (Instruction*)POP();
				frame_ptr       = (intptr_t*)   POP();

				if (program_counter == 0)
					goto return_main;
			} continue;

			case OP_JMP: {
				program_counter     = dst_val + code;
			} continue;
			case OP_JNZ: {
				if (SRC != 0)
					program_counter = dst_val + code;
			} continue;
			case OP_JZ: {
				if (SRC == 0)
					program_counter = dst_val + code;
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

			default:
				assert(false);
		}
	}

return_main:
	assert(frame_ptr == stack);
	assert(stack_ptr == stack);
}

struct VM {
	const size_t stack_size = 1024 * 64;
	int64_t* stack = new int64_t[stack_size];

	~VM () {
		delete stack;
	}

	void execute (Instruction* code, size_t code_sz, size_t entry_point) {
	#ifndef TRACY_ENABLE
		printf("--------------------------------------------------------------------------------\n");
		printf("VM execute:\n");
	#endif

		_execute(code, code_sz, stack, stack_size, entry_point);
	}
};
