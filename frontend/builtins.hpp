#pragma once
#include "common.hpp"
#include "basic_types.hpp"
#include "parser.hpp"
#include "types.hpp"

#include "stdarg.h"

inline void print (bool b) {
	printf("%s", b ? "true":"false");
}
inline void print (int64_t i) {
	printf("%" PRIi64, i);
}
inline void print (double f) {
	printf("%f", f);
}
inline void print (const char* str) {
	printf("%s", str);
}

inline void my_printf (const char* format, ...) {
#ifdef TRACY_ENABLE  // disable prints for profiling
	return;
#endif

	va_list varargs;
	va_start(varargs, format);

	const char* cur = format;

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
				
				switch (params[0]) {
					case 'b': print( va_arg(varargs, bool       ) ); break;
					case 'i': print( va_arg(varargs, int64_t    ) ); break;
					case 'f': print( va_arg(varargs, double     ) ); break;
					case 's': print( va_arg(varargs, char const*) ); break;
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

	va_end(varargs);
}

inline int64_t timer () {
	return get_timestamp();
}
inline double timer_end (int64_t start) {
	auto end = (int64_t)get_timestamp();
	return (double)(end - start) / (double)timestamp_freq;
}

////

// printf
inline AST_vardecl bf_printf_val     = { cAST(A_VARDECL, pTY_STR), "format" };
inline AST_vardecl bf_printf_varargs = { cAST(A_VARARGS         ), "args" };

inline AST_vardecl* bf_printf_args[] = { &bf_printf_val, &bf_printf_varargs };
inline AST_funcdef bf_printf = {
	cAST(A_FUNCDEF), "printf", bf_printf_args, {}, nullptr, &my_printf
};

// timer
inline AST_vardecl bf_timer_ret = { cAST(A_VARDECL, pTY_INT), "timestamp" };

inline AST_vardecl* bf_timer_rets[] = { &bf_timer_ret };
inline AST_funcdef bf_timer = {
	cAST(A_FUNCDEF), "timer", {}, bf_timer_rets, nullptr, &timer
};

// timer_end
inline AST_vardecl bf_timer_end_start = { cAST(A_VARDECL, pTY_INT), "start_timestamp" };
inline AST_vardecl bf_timer_end_ret   = { cAST(A_VARDECL, pTY_FLT), "timestamp" };

inline AST_vardecl* bf_timer_end_args[] = { &bf_timer_end_start };
inline AST_vardecl* bf_timer_end_rets[] = { &bf_timer_end_ret };
inline AST_funcdef bf_timer_end = {
	cAST(A_FUNCDEF), "timer_end", bf_timer_end_args, bf_timer_end_rets, nullptr, &timer_end
};

inline AST_funcdef* builtin_funcs[] = {
	&bf_printf,
	&bf_timer,
	&bf_timer_end,
};
