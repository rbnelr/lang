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
					case 'b': print( (bool)va_arg(varargs, int        ) ); break;
					case 'i': print(       va_arg(varargs, int64_t    ) ); break;
					case 'f': print(       va_arg(varargs, double     ) ); break;
					case 's': print(       va_arg(varargs, char const*) ); break;
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

struct BuiltinArg {
	strview   name;
	AST_type* basic_type = nullptr; // null -> varargs
};
struct BuiltinFuncBuf {
	static inline constexpr int MAX_ARGS = 16;

	AST_funcdef fdef = {};

	AST_vardecl* arg_ptrs[MAX_ARGS] = {};
	AST_vardecl* ret_ptrs[MAX_ARGS] = {};

	AST_vardecl arg_decls[MAX_ARGS] = {};
	AST_vardecl ret_decls[MAX_ARGS] = {};
	
	// Source ranges stay zero, since no source text to point to

	BuiltinFuncBuf (strview name, void* func_ptr, std::initializer_list<BuiltinArg> args, std::initializer_list<BuiltinArg> rets) {
		auto* func = &fdef;

		func->kind = A_FUNCDEF;
		func->src = SourceRange{}; // no source text to point to
		func->ident = name;

		auto get_args = [] (std::initializer_list<BuiltinArg>& args, AST_vardecl* out, AST_vardecl** out_ptrs) -> arrview<AST_vardecl*> {
			size_t i = 0;

			for (auto& arg : args) {
				assert(i < BuiltinFuncBuf::MAX_ARGS);
				auto* decl = &out[i];
				out_ptrs[i++] = decl;

				decl->kind = arg.basic_type ? A_VARDECL : A_VARARGS;
				decl->ident = arg.name;
			
				if (arg.basic_type)
					decl->type  = Typeref::RValue( arg.basic_type );
			}

			return { out_ptrs, i };
		};

		func->args = get_args(args, arg_decls, arg_ptrs);
		func->rets = get_args(rets, ret_decls, ret_ptrs);

		assert(func->rets.count <= 1);
		//func->ret_struct = ast_alloc<AST_structdef>(A_STRUCTDEF);
		//func->ret_struct->src = SourceRange{}; // no source text to point to
		//func->ret_struct->ident = format("%.*s.Result", (int)func->ident.size(), func->ident.data());
		//func->ret_struct->members = rets;
		//
		//// Create type for return struct
		//func->ret_struct_ty = ast_alloc<AST_type>(A_TYPE);
		//func->ret_struct_ty->tclass = TY_STRUCT;
		//func->ret_struct_ty->ident  = func->ret_struct->ident;
		//func->ret_struct_ty->decl   = func->ret_struct;
		//func->ret_struct_ty->src    = func->ret_struct->src;

		func->builtin_func_ptr = func_ptr;
	}
};

inline BuiltinFuncBuf _printf    { "printf"   , (void*)my_printf, {{"format", pTY_STR},{"args"}}, {} };
inline BuiltinFuncBuf _timer     { "timer"    , (void*)timer    , {}, {{"timestamp", pTY_INT}} };
inline BuiltinFuncBuf _timer_end { "timer_end", (void*)timer_end, {{"start_timestamp", pTY_INT}},{{"period", pTY_FLT}} };

inline AST_funcdef* BUILTIN_FUNCS[] = {
	&_printf   .fdef,
	&_timer    .fdef,
	&_timer_end.fdef,
};
