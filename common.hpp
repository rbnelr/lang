#pragma once

#include "stdio.h"
#include "math.h"
#include "assert.h"
#include "string.h"
#include "stdlib.h"

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>

#include "util/macros.hpp"
#include "util/string.hpp"
#include "util/file_io.hpp"
#include "util/console_colors.hpp"
#include "util/timer.hpp"
#include "util/smhasher/MurmurHash2.h"

#include <exception>

#include "inttypes.h" // for PRIuMAX for size_t printf'ing

#include "Tracy.hpp"

#include "options.hpp"

using namespace kiss;

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

#define _DBG_MAGIC_NONALLOC 0xcc
#define _DBG_MAGIC_UNINIT   0xcd
#define _DBG_MAGIC_FREED    0xdd

struct BumpAllocator {
	static inline constexpr size_t BLOCK_SZ = 1024 * 1024 * 1;

	char* cur = nullptr;
	char* end = nullptr;

	std::vector<char*> blocks;

	BumpAllocator () {
		blocks.reserve(32);
	}
	~BumpAllocator () {
		reset();
	}

	static inline void* align_ptr (void* p, size_t align) {
		return (void*)( ((uintptr_t)p + (align-1)) & ~(align-1) );
	}

	// new block is needed, slow path so use _NOINLINE to help the compiler pick the fast path for inlining
	_NOINLINE char* alloc_from_new_block (size_t size, size_t align) {
		ZoneScoped;

		add_block();

		cur = (char*)align_ptr(cur, align);

		char* ptr = cur;
		cur += size;

		if (cur <= end)
			return ptr;

		assert(false);
		return nullptr;
	}
	inline char* alloc (size_t size, size_t align) {
		// code would work fine with size=0, but maybe avoid returning a pointer to _no_ data
		// esp. since it actually does align the pointer to align and
		// potentially triggers new block allocation for no reason
		if (size == 0)
			return nullptr;

		cur = (char*)align_ptr(cur, align);

		char* ptr = cur;
		cur += size;

		if (cur <= end)
			return ptr;

		return alloc_from_new_block(size, align);
	}

	template <typename T>
	T* alloc () {
		T* ptr = (T*)alloc(sizeof(T), alignof(T));
		new (ptr) T ();
		return ptr;
	}

	// WARNING: ctor not called due to performance concerns
	template <typename T>
	T* alloc_array (size_t count) {
		return (T*)alloc(sizeof(T)*count, alignof(T));
	}

	void add_block () {
		cur = (char*)std::malloc(BLOCK_SZ);
		end = cur + BLOCK_SZ;

		blocks.push_back(cur);
	}
	void reset () {
		for (auto& ptr : blocks)
			std::free(ptr);

		blocks.clear();

		cur = nullptr;
		end = nullptr;
	}
};

inline BumpAllocator g_allocator;

////
typedef std::string_view strview;

// g_allocator-backed buffer printf
inline char const* format (char const* format, ...) {
	va_list vl;
	va_start(vl, format);

	int required_len = vsnprintf(nullptr, 0, format, vl);
	if (required_len <= 0)
		return nullptr; // error or empty string

	char* str = g_allocator.alloc_array<char>(required_len + 1); // required_len is exluding the null terminator

	int written_len = vsnprintf(str, required_len + 1, format, vl);
	if (written_len != required_len)
		return nullptr; // error?

	va_end(vl);

	return str;
}

inline void print_seperator (strview str, char fill_char='=') {
#ifdef TRACY_ENABLE // disable prints for profiling
	return;
#endif

	constexpr int LEN = 80;
	constexpr int SPACE = 1;

	int pad = LEN - SPACE*2 - (int)str.size();
	pad = std::max(pad, 0);

	int padL = pad/2;
	int padR = pad - padL;

	for (int i=0; i<padL; ++i)  putchar(fill_char);
	for (int i=0; i<SPACE; ++i) putchar(' ');

	printf("%.*s", (int)str.size(), str.data());
	
	for (int i=0; i<SPACE; ++i) putchar(' ');
	for (int i=0; i<padR; ++i)  putchar(fill_char);

	putchar('\n');
}

// just for printing string to the console which might contain newlines or nulls
inline std::string escape_string_capped (strview str, size_t max_len=(size_t)-1) {
	std::string out;
	out.reserve(str.size() + 8); // should prevent reallocs most of the time

	size_t i = 0;
	for (; i < max_len && i < str.size(); ++i) {
		switch (str[i]) {
			//case '\\': out.append("\\\\", 2); break;
			case '\n': out.append("\\n", 2); break;
			case '\r': out.append("\\r", 2); break;
			case '\0': out.append("\\0", 2); break;
				//case '"' : out.append("\\\"", 2); break;
			default: out.push_back(str[i]);
		}
	}
	if (i != str.size()) {
		out.append("...", 3);
	}

	return out;
}

template <typename T>
inline void grow (std::vector<T>& vec, size_t min_sz) {
	if (vec.size() < min_sz) {
		vec.resize(min_sz);
	}
}

#define TRACY_REPEAT 100
