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
			case OPC_NOP: {

			} continue;

			case OPC_SPUSH: {
				PUSHN(dst_val);
			} continue;
			case OPC_SPOP: {
				POPN(dst_val);
			} continue;

			case OPC_PUSH: {
				PUSH(SRC);
			} continue;
			case OPC_PUSH | OPC_IMM: {
				PUSH(src_val);
			} continue;
			case OPC_POP: {
				DST = POP();
			} continue;

			case OPC_MOV: {
				DST = SRC;
			} continue;
			case OPC_MOV | OPC_IMM: {
				DST = src_val;
			} continue;

			case OPC_CALLB: {
				auto* builtin_func = (builtin_func_t)dst_val;
				auto* argptr = (Value*)(stack_ptr-1);

				//builtin_func(argptr);
			} continue;

			case OPC_CALL: {
				PUSH((intptr_t)frame_ptr);
				PUSH((intptr_t)program_counter);

				frame_ptr       = stack_ptr;
				program_counter = dst_val + code;
			} continue;

			case OPC_RET: {
				program_counter = (Instruction*)POP();
				frame_ptr       = (intptr_t*)   POP();

				if (program_counter == 0)
					goto return_main;
			} continue;

			case OPC_JMP: {
				program_counter     = dst_val + code;
			} continue;
			case OPC_JNZ: {
				if (SRC != 0)
					program_counter = dst_val + code;
			} continue;
			case OPC_JZ: {
				if (SRC == 0)
					program_counter = dst_val + code;
			} continue;

			case OPC_NEG: {
				auto& i = DST;
				i = -i;
			} continue;
			case OPC_NOT: {
				auto& i = DST;
				i = !i;
			} continue;

			case OPC_INC              : DST++;                                      continue;
			case OPC_DEC              : DST--;                                      continue;

			case OPC_ADD              : DST += SRC;                                 continue;
			case OPC_SUB              : DST -= SRC;                                 continue;
			case OPC_MUL              : DST *= SRC;                                 continue;
			case OPC_DIV              : DST /= SRC;                                 continue;
			case OPC_REMAIND          : DST %= SRC;                                 continue;
			case OPC_ADD     | OPC_IMM : DST += (int64_t)src_val;                    continue;
			case OPC_SUB     | OPC_IMM : DST -= (int64_t)src_val;                    continue;
			case OPC_MUL     | OPC_IMM : DST *= (int64_t)src_val;                    continue;
			case OPC_DIV     | OPC_IMM : DST /= (int64_t)src_val;                    continue;
			case OPC_REMAIND | OPC_IMM : DST %= (int64_t)src_val;                    continue;

			case OPC_LT               : DST = (int64_t)(DST <  SRC);                continue;
			case OPC_LE               : DST = (int64_t)(DST <= SRC);                continue;
			case OPC_GT               : DST = (int64_t)(DST >  SRC);                continue;
			case OPC_GE               : DST = (int64_t)(DST >= SRC);                continue;
			case OPC_EQ               : DST = (int64_t)(DST == SRC);                continue;
			case OPC_NEQ              : DST = (int64_t)(DST != SRC);                continue;
			case OPC_LT      | OPC_IMM : DST = (int64_t)(DST <  (int64_t)src_val);   continue;
			case OPC_LE      | OPC_IMM : DST = (int64_t)(DST <= (int64_t)src_val);   continue;
			case OPC_GT      | OPC_IMM : DST = (int64_t)(DST >  (int64_t)src_val);   continue;
			case OPC_GE      | OPC_IMM : DST = (int64_t)(DST >= (int64_t)src_val);   continue;
			case OPC_EQ      | OPC_IMM : DST = (int64_t)(DST == (int64_t)src_val);   continue;
			case OPC_NEQ     | OPC_IMM : DST = (int64_t)(DST != (int64_t)src_val);   continue;

			case OPC_FNEG: {
				auto& d = FDST;
				d = -d;
			} continue;

			case OPC_FADD             : FDST += FSRC;                               continue;
			case OPC_FSUB             : FDST -= FSRC;                               continue;
			case OPC_FMUL             : FDST *= FSRC;                               continue;
			case OPC_FDIV             : FDST /= FSRC;                               continue;
			case OPC_FADD    | OPC_IMM : FDST += *(double*)&op.src;                  continue;
			case OPC_FSUB    | OPC_IMM : FDST -= *(double*)&op.src;                  continue;
			case OPC_FMUL    | OPC_IMM : FDST *= *(double*)&op.src;                  continue;
			case OPC_FDIV    | OPC_IMM : FDST /= *(double*)&op.src;                  continue;

			case OPC_FLT              : DST = (int64_t)(FDST <  FSRC);              continue;
			case OPC_FLE              : DST = (int64_t)(FDST <= FSRC);              continue;
			case OPC_FGT              : DST = (int64_t)(FDST >  FSRC);              continue;
			case OPC_FGE              : DST = (int64_t)(FDST >= FSRC);              continue;
			case OPC_FEQ              : DST = (int64_t)(FDST == FSRC);              continue;
			case OPC_FNEQ             : DST = (int64_t)(FDST != FSRC);              continue;
			case OPC_FLT     | OPC_IMM : DST = (int64_t)(FDST <  *(double*)&op.src); continue;
			case OPC_FLE     | OPC_IMM : DST = (int64_t)(FDST <= *(double*)&op.src); continue;
			case OPC_FGT     | OPC_IMM : DST = (int64_t)(FDST >  *(double*)&op.src); continue;
			case OPC_FGE     | OPC_IMM : DST = (int64_t)(FDST >= *(double*)&op.src); continue;
			case OPC_FEQ     | OPC_IMM : DST = (int64_t)(FDST == *(double*)&op.src); continue;
			case OPC_FNEQ    | OPC_IMM : DST = (int64_t)(FDST != *(double*)&op.src); continue;

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
