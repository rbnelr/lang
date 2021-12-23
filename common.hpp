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

using namespace kiss;

typedef std::string_view strview;


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

#define INVALID_DEFAULT assert(false); _UNREACHABLE

#if 1
	#define TRACY_REPEAT 100000
#else
	#define TRACY_REPEAT 100
#endif

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
		cur = (char*)align_ptr(cur, align);

		char* ptr = cur;
		cur += size;

		if (cur <= end)
			return ptr;

		return alloc_from_new_block(size, align);
	}

	template <typename T>
	T* alloc () {
		return (T*)alloc(sizeof(T), alignof(T));
	}
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

BumpAllocator g_allocator;

// g_allocator-backed buffer printf
char const* format (char const* format, ...) {
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
