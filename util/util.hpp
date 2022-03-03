#pragma once
#include "stdint.h"
#include "assert.h"

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

////
// Get length of fixed-size (C-style) array
// be careful! don't use with pointers, only directly with arrays (not std::vectors either)
#define ARRLEN(arr) (sizeof(arr) / sizeof(arr[0]))

#define TO_STRING(x) #x

////
#define _DBG_MAGIC_NONALLOC 0xcc
#define _DBG_MAGIC_UNINIT   0xcd
#define _DBG_MAGIC_FREED    0xdd

////
inline constexpr size_t KB = (size_t)1024;
inline constexpr size_t MB = (size_t)1024 * 1024;
inline constexpr size_t GB = (size_t)1024 * 1024 * 1024;
inline constexpr size_t TB = (size_t)1024 * 1024 * 1024 * 1024;

_FORCEINLINE bool is_POT (size_t x) {
	return x != 0 && ((x-1) & x) == 0;
}
_FORCEINLINE uintptr_t align_up (uintptr_t x, uintptr_t align) {
	assert(is_POT(align));
	return (x + (align-1)) & ~(align-1);
}
_FORCEINLINE char* align_up (char* ptr, uintptr_t align) {
	return (char*)align_up((uintptr_t)ptr, align);
}

////
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


////// Helper macros for declaring classes containing resources that can not be copied
// useful for opengl textures, buffers, etc., or other resources like memory buffers, that you would rather copy explicitly
// basicly any class that contains a raw pointer or system handle where copying the class would cause the ptr to be copied which is wrong since there will probably now be a double free or no free at all

// default constructor should result in "empty" class that still destructs, but the destructor does nothing since the class is empty, this is useful
// no copy ctor/assign, only move
// use like:
/*
	class T {
		void* my_resource = nullptr;
	public:
		MOVE_ONLY_CLASS(T) // move operators implemented with swap

		~T () {
			// destructor can destruct default constructed class
			free(my_resource); // free(nullptr) is ok
			// or
			if (my_resource)
				api_delete(my_resource);
		}
		static T my_factory (args...) {
			T t;
			// construct a T
			return t;
		}
	};
	void swap (T& l, T& r) {
		std::swap(l.my_resource, r.my_resource);
	}
*/
#define MOVE_ONLY_CLASS(CLASS) \
	public: \
	friend void swap (CLASS& l, CLASS& r); \
	CLASS& operator= (CLASS& r) = delete; \
	CLASS (CLASS& r) = delete; \
	CLASS& operator= (CLASS&& r) {	swap(*this, r);	return *this; } \
	CLASS (CLASS&& r) {				swap(*this, r); } \
	private:

#define MOVE_ONLY_CLASS_MEMBER(CLASS, CLSMEMB) \
	friend void swap (CLASS& l, CLASS& r) { std::swap(l.CLSMEMB, r.CLSMEMB); } \
	CLASS& operator= (CLASS& r) = delete; \
	CLASS (CLASS& r) = delete; \
	CLASS& operator= (CLASS&& r) {	swap(*this, r);	return *this; } \
	CLASS (CLASS&& r) {				swap(*this, r); }

#define MOVE_ONLY_CLASS_DECL(CLASS) \
	friend void swap (CLASS& l, CLASS& r); \
	CLASS& operator= (CLASS& r) = delete; \
	CLASS (CLASS& r) = delete; \
	CLASS& operator= (CLASS&& r); \
	CLASS (CLASS&& r);
#define MOVE_ONLY_CLASS_DEF(CLASS) \
	CLASS& CLASS::operator= (CLASS&& r) {	swap(*this, r);	return *this; } \
	CLASS::CLASS (CLASS&& r) {				swap(*this, r); }

// For classes that cannot be copied or moved at all, for example because they contain data that has to stay allocated at the same address (eg. ReadDirectoryChangesW in overlapped mode needs a pointer to a buffer)
// Can still pass the class around by allocating it with new or make_unique
#define NO_MOVE_COPY_CLASS(CLASS) \
	CLASS& operator= (CLASS& r) = delete; \
	CLASS (CLASS& r) = delete; \
	CLASS& operator= (CLASS&& r) = delete; \
	CLASS (CLASS&& r) = delete;

// Enumeration bit operators, for using enums as bitfields (very useful because visual studio shows them like "VAL1(1) | VAL2(8) | 128")
#define ENUM_BITFLAG_OPERATORS_TYPE(e, itype) \
	inline constexpr e operator| (e l, e r) { return (e)((itype)l | (itype)r); } \
	inline constexpr e operator& (e l, e r) { return (e)((itype)l & (itype)r); } \
	inline constexpr e operator^ (e l, e r) { return (e)((itype)l ^ (itype)r); } \
	inline constexpr e operator~ (e l) { return (e)(~(itype)l); } \
	inline e& operator|= (e& l, e r) { return l = (e)((itype)l | (itype)r); } \
	inline e& operator&= (e& l, e r) { return l = (e)((itype)l & (itype)r); } \
	inline e& operator^= (e& l, e r) { return l = (e)((itype)l ^ (itype)r); }

#define ENUM_BITFLAG_OPERATORS(e) ENUM_BITFLAG_OPERATORS_TYPE(e, int)

