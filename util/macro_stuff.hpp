#pragma once

#define STRINGIFY(x) #x

//// Preprocessor stuff
#ifdef __GNUC__ // GCC 4.8+, Clang, Intel and other compilers compatible with GCC (-std=c++0x or above)
	#define _ASSUME(cond) if (!(cond)) __builtin_unreachable()
	#define _UNREACHABLE __builtin_unreachable()
	#define _FORCEINLINE __attribute__((always_inline)) inline
	#define _NOINLINE    __attribute__((noinline))

#elif defined(_MSC_VER) // MSVC
	#define _ASSUME(cond) __assume(cond)
	#define _UNREACHABLE  __assume(false)
	#define _FORCEINLINE  __forceinline
	#define _NOINLINE     __declspec(noinline)
#else
	#define _ASSUME(cond)
	#define _UNREACHABLE  
	#define _FORCEINLINE  
	#define _NOINLINE     
#endif

template <typename Func> struct _Defer {
	Func func;
	_Defer(Func func) : func(func) {}
	~_Defer() { func(); }
};

template <typename Func>
_Defer<Func> _defer (Func func) {
	return _Defer<Func>(func);
}

#define DEFER_1(A, B) A ## B
#define DEFER_2(A, B) DEFER_1(A, B)
#define DEFER_3(A)    DEFER_2(A, __COUNTER__)

// use like:
//  defer( statement; );
// or
//  defer(
//      statement;
//      statement;
//      ...
//  );
#define defer(code)   auto DEFER_3(_defer_) = _defer([&] () { code })

#define INVALID_DEFAULT default: { assert(false); _UNREACHABLE; } break
