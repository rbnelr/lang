#pragma once
#include "stdio.h"
#include "math.h"
#include "assert.h"
#include "string.h"
#include "stdlib.h"
#include "inttypes.h" // for PRIuMAX for size_t printf'ing

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <exception>

#include "util/macros.hpp"
#include "util/string.hpp"
#include "util/file_io.hpp"
#include "util/console_colors.hpp"
#include "util/timer.hpp"
#include "util/smhasher/MurmurHash2.h"
#include "util/macro_stuff.hpp"
#include "util/allocators.hpp"

#include "Tracy.hpp"

#include "options.hpp"

using namespace kiss;

inline constexpr size_t KB = (size_t)1024;
inline constexpr size_t MB = (size_t)1024 * 1024;
inline constexpr size_t GB = (size_t)1024 * 1024 * 1024;
inline constexpr size_t TB = (size_t)1024 * 1024 * 1024 * 1024;

////
typedef std::string_view strview;

inline BumpAllocator g_allocator;

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
