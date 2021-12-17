#pragma once
#include "common.hpp"
#include "value.hpp"
#include "parser.hpp"

inline constexpr AST cAST (ASTType type, AST* next=nullptr) {
	return { type, {nullptr,nullptr}, next };
}

void print (Value* vals, size_t valc) {
	Value& v = vals[0];
	switch (v.type) {
		case NULL:
			printf("null");
			break;
		case BOOL:
			printf("%s", v.u.b ? "true":"false");
			break;
		case INT:
			printf("%" PRIi64, v.u.i);
			break;
		case FLT:
			printf("%f", v.u.f);
			break;
		case STR:
			printf("%s", v.u.str);
			break;
		default:
			assert(false);
	}
}
inline AST_vardecl BF_PRINT_VAL = { cAST(A_VARDECL), "val", 0 };
inline AST_funcdef_builtin BF_PRINT = {
	cAST(A_FUNCDEF_BUILTIN), "print", 1, (AST*)&BF_PRINT_VAL, 0, nullptr, print
};

void my_printf (Value* vals, size_t valc) {
	auto& format = vals[0];

	auto* varargs = &vals[1];
	auto  varargc = valc-1;

	const char* cur = format.u.str;

	size_t i = 0;
	while (*cur != '\0') {
		if (*cur == '{') {
			const char* params = cur++;

			while (*cur != '}')
				cur++;
			cur++;

			if (i >= varargc)
				// print null for % that access outside of the varargs
				print((Value*)&NULLVAL, 1); // cast away const, since unified args/rets force me to not use const in print_val
			else
				print(&varargs[i++], 1);
			continue;
		} else {
			if (cur[0] == '^' && cur[1] == '{') {
				cur++; // ^{ escape sequence
			}
			putc(*cur++, stdout);
		}
	}
	// ignore varargs that are not printed (no error)
}
inline AST_vardecl BF_PRINTF_VARARGS = { cAST(A_VARARGS                          ), "args", 1 };
inline AST_vardecl BF_PRINTF_VAL     = { cAST(A_VARDECL, (AST*)&BF_PRINTF_VARARGS), "format", 0 };

inline AST_funcdef_builtin BF_PRINTF = {
	cAST(A_FUNCDEF_BUILTIN), "printf", 2, (AST*)&BF_PRINTF_VAL, 0, nullptr, my_printf
};

void timer (Value* vals, size_t valc) {
	vals[0] = (int64_t)get_timestamp();
}
inline AST_vardecl BF_TIMER_RET = { cAST(A_VARDECL), "timestamp", 0 };

inline AST_funcdef_builtin BF_TIMER = {
	cAST(A_FUNCDEF_BUILTIN), "timer", 0, nullptr, 1, (AST*)&BF_TIMER_RET, timer
};

void timer_end (Value* vals, size_t valc) {
	auto start = vals[1];
	auto end = (int64_t)get_timestamp();
	vals[0] = (double)(end - start.u.i) / (double)timestamp_freq;
}
inline AST_vardecl BF_TIMER_END_START = { cAST(A_VARDECL), "start_timestamp", 1 };
inline AST_vardecl BF_TIMER_END_RET   = { cAST(A_VARDECL), "timestamp", 0 };

inline AST_funcdef_builtin BF_TIMER_END = {
	cAST(A_FUNCDEF_BUILTIN), "timer_end", 1, (AST*)&BF_TIMER_END_START, 1, (AST*)&BF_TIMER_END_RET, timer_end
};

inline const AST_funcdef_builtin* BUILTIN_FUNCS[] = {
	&BF_PRINT,
	&BF_PRINTF,
	&BF_TIMER,
	&BF_TIMER_END,
};
