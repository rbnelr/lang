#pragma once
#include "common.hpp"
#include "basic_types.hpp"
#include "parser.hpp"

inline constexpr AST cAST (ASTType type, Type valtype=VOID, AST* next=nullptr) {
	return { type, nullptr, next, valtype };
}

void print (bool b) {
	printf("%s", b ? "true":"false");
}
void print (int64_t i) {
	printf("%" PRIi64, i);
}
void print (double f) {
	printf("%f", f);
}
void print (const char* str) {
	printf("%s", str);
}

void my_printf (Value* vals) {
#ifdef TRACY_ENABLE  // disable prints for profiling
	return;
#endif

	auto& format = vals[0];

	auto* varargs = &vals[-1];
	auto get_arg = [&] (size_t idx) -> Value& {
		return varargs[-(intptr_t)idx];
	};

	const char* cur = format.str;

	size_t i = 0;
	while (*cur != '\0') {
		if (*cur == '{') {
			const char* start = ++cur;

			while (*cur != '}')
				cur++;
			const char* end = cur++;

			strview params = strview(start, end - start);

			//if (i >= varargc) {
			//	// print null for % that access outside of the varargs
			//	printf("null"); // cast away const, since unified args/rets force me to not use const in print_val
			//}
			//else {
				if (params.size() != 1)
					throw RuntimeExcept{"runtime error: printf: expected type specifier"};
				
				auto& val = get_arg(i++);

				switch (params[0]) {
					case 'b': print(val.b);   break;
					case 'i': print(val.i);   break;
					case 'f': print(val.f);   break;
					case 's': print(val.str); break;
					default:
						throw RuntimeExcept{"runtime error: printf: unknown type specifier"};
				}
			//}
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
inline AST_vardecl BF_PRINTF_VARARGS = { cAST(A_VARARGS                               ), "args" };
inline AST_vardecl BF_PRINTF_VAL     = { cAST(A_VARDECL, STR, (AST*)&BF_PRINTF_VARARGS), "format" };

inline AST_funcdef_builtin BF_PRINTF = {
	cAST(A_FUNCDEF_BUILTIN), "printf", 2, &BF_PRINTF_VAL, 0, nullptr, my_printf
};

void timer (Value* vals) {
	vals[0].i = (int64_t)get_timestamp();
}
inline AST_vardecl BF_TIMER_RET = { cAST(A_VARDECL, INT), "timestamp" };

inline AST_funcdef_builtin BF_TIMER = {
	cAST(A_FUNCDEF_BUILTIN), "timer", 0, nullptr, 1, &BF_TIMER_RET, timer
};

void timer_end (Value* vals) {
	auto start = vals[-1];
	auto end = (int64_t)get_timestamp();
	vals[0].f = (double)(end - start.i) / (double)timestamp_freq;
}
inline AST_vardecl BF_TIMER_END_START = { cAST(A_VARDECL, INT), "start_timestamp" };
inline AST_vardecl BF_TIMER_END_RET   = { cAST(A_VARDECL, FLT), "timestamp" };

inline AST_funcdef_builtin BF_TIMER_END = {
	cAST(A_FUNCDEF_BUILTIN), "timer_end", 1, &BF_TIMER_END_START, 1, &BF_TIMER_END_RET, timer_end
};

inline const AST_funcdef_builtin* BUILTIN_FUNCS[] = {
	&BF_PRINTF,
	&BF_TIMER,
	&BF_TIMER_END,
};
