#pragma once
#include "common.hpp"
#include "basic_types.hpp"
#include "parser.hpp"
#include "types.hpp"

#include "stdarg.h"

struct _FormatOpt {
	size_t pad_width;
	char   pad_dir;  // only valid if pad_width > 0
	char   pad_char; // only valid if pad_width > 0
};

inline _NOINLINE void print (const char* str, size_t width, _FormatOpt& opt) {
	size_t padl = 0;
	size_t padr = 0;
	if (opt.pad_width > width) {
		size_t total_pad = opt.pad_width - width;
		padl = total_pad / 2;
		padr = total_pad - padl;
	}
	
	for (size_t i=0; i<padl; ++i)
		putc(opt.pad_char, stdout);

	fputs(str, stdout);

	for (size_t i=0; i<padr; ++i)
		putc(opt.pad_char, stdout);
}

inline void print (int64_t i,       _FormatOpt& opt) {
	char buf[64];
	int len = snprintf(buf, ARRLEN(buf), "%" PRId64, i);
	if (len >= ARRLEN(buf)) {
		assert(false);
		return; // should be impossible
	}
	
	print(buf, len, opt);
}
inline void print (double f,        _FormatOpt& opt) {
	char buf[64];
	int len = snprintf(buf, ARRLEN(buf), "%f", f);
	if (len >= ARRLEN(buf)) {
		assert(false);
		return; // should be impossible
	}
	
	print(buf, len, opt);
}
inline void print (const char* str, _FormatOpt& opt) {
	size_t len = strlen(str);
	
	print(str, len, opt);
}

inline bool _parse_int (const char*& str, size_t* out_int) {
	const char* cur = str;

	if (!(*cur >= '0' && *cur <= '9'))
		return false;

	size_t i = 0;
	do {
		int digit = *cur++ - '0';
		i *= 10;
		i += digit;
	} while (*cur >= '0' && *cur <= '9');

	*out_int = i;
	str = cur;
	return true;
}

inline _FormatOpt _get_format_padding (const char*& format) {
	const char* cur = format;

	_FormatOpt opt;
	opt.pad_width = 0;
	opt.pad_dir = '>';
	opt.pad_char = ' ';

	if (cur[0] == '}') return opt; // format string over
	
	if (cur[0] == '>' || cur[0] == '<' || cur[0] == '^') {
		opt.pad_dir = *cur++;
	}
	
	if (!(cur[0] > '0' && cur[0] <= '9')) {
		opt.pad_char = *cur++;
	}


	if (!_parse_int(cur, &opt.pad_width)) {
		assert(opt.pad_width == 0);
		return opt;
	}

	format = cur; // only if width int appeared is padding char valid
	return opt;
}

inline void my_printf (const char* format, ...) {
#ifdef TRACY_ENABLE  // disable prints for profiling
	return;
#endif

	va_list vl;
	va_start(vl, format);

	const char* cur = format;

	while (*cur != '\0') {
		if (*cur == '{') {
			cur++;

			_FormatOpt opt = _get_format_padding(cur);

			switch (*cur++) {
				case 'b': print( (bool)va_arg(vl, int        ) ? "true":"false", opt); break;
				case 'i': print(       va_arg(vl, int64_t    )                 , opt); break;
				case 'f': print(       va_arg(vl, double     )                 , opt); break;
				case 's': print(       va_arg(vl, char const*)                 , opt); break;
				default:
					throw RuntimeExcept{"runtime error: printf: unknown type specifier"};
			}
			
			if (*cur != '}')
				throw RuntimeExcept{"runtime error: printf: expected '}' after '{'"};
			cur++;
		}
		else {
			if (cur[0] == '^' && cur[1] == '{') {
				cur++; // ^{ escape sequence
			}
			putc(*cur++, stdout);
		}
	}
	// ignore varargs that are not printed (no error)

	va_end(vl);
}

inline int64_t timer () {
	return get_timestamp();
}
inline double timer_end (int64_t start) {
	auto end = (int64_t)get_timestamp();
	return (double)(end - start) / (double)timestamp_freq;
}

////

// Ugly hack to simplify making ASTs for 'builtin' functions
// Later might see if I can't do this simply via an actual standard library source file
// that just happens to be turned into (LLVM) bitcode that can be quickly linked
// though unsure how to make a source file that defines functions that are actually things I 'link' to, ie. functions implemented in the JIT code
// Don't really want to rely on name-based linking which LLVM would allow

struct BuiltinArg {
	strview   name;
	AST_type* basic_type = nullptr; // null -> varargs
};
struct BuiltinFuncBuf {
	static inline constexpr int MAX_ARGS = 16;

	AST_funcdef fdef = {};

	AST_func_arg* arg_ptrs[MAX_ARGS] = {};
	AST_func_arg* ret_ptrs[MAX_ARGS] = {};

	struct Arg {
		AST_func_arg arg;
		AST_vardecl  decl;
	};

	Arg arg_decls[MAX_ARGS] = {};
	Arg ret_decls[MAX_ARGS] = {};
	
	// Source ranges stay zero, since no source text to point to

	BuiltinFuncBuf (strview name, void* func_ptr, std::initializer_list<BuiltinArg> args, std::initializer_list<BuiltinArg> rets) {
		auto* func = &fdef;

		func->kind = A_FUNCDECL;
		func->src = SourceRange{}; // no source text to point to
		func->ident = name;

		auto get_args = [] (std::initializer_list<BuiltinArg>& args, Arg* out, AST_func_arg** out_ptrs) -> arrview<AST_func_arg*> {
			size_t i = 0;

			for (auto& arg : args) {
				assert(i < BuiltinFuncBuf::MAX_ARGS);
				out[i].arg.decl = &out[i].decl;
				out_ptrs[i] = &out[i].arg;

				out[i].arg.kind = A_FUNCARG;
				
				out[i].decl.kind = arg.basic_type ? A_VARDECL : A_VARARGS;
				out[i].decl.ident = arg.name;
				if (arg.basic_type)
					out[i].decl.type  = Typeref::RValue( arg.basic_type );

				i++;
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
