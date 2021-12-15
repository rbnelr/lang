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

#include "util/string.hpp"
#include "util/file_io.hpp"
#include "util/console_colors.hpp"
#include "util/timer.hpp"
#include "util/smhasher/MurmurHash2.h"

#include <exception>

#include "inttypes.h" // for PRIuMAX for size_t printf'ing

#include "Tracy.hpp"

#define ARRLEN(arr) (sizeof(arr) / sizeof((arr)[0]))

using namespace kiss;

typedef std::string_view strview;


#ifdef __GNUC__ // GCC 4.8+, Clang, Intel and other compilers compatible with GCC (-std=c++0x or above)
	#define _ASSUME(cond) if (!(cond)) __builtin_unreachable()
	#define _UNREACHABLE __builtin_unreachable()
	#define _FORCEINLINE __attribute__((always_inline)) inline

#elif defined(_MSC_VER) // MSVC
	#define _ASSUME(cond) __assume(cond)
	#define _UNREACHABLE  __assume(false)
	#define _FORCEINLINE  __forceinline
#else
	#define _ASSUME(cond)
	#define _UNREACHABLE  
	#define _FORCEINLINE 
#endif

#if 1
#define TRACY_REPEAT 100000
#define TRACY_TRACK_ALLOC 0
#else
#define TRACY_REPEAT 10
#define TRACY_TRACK_ALLOC 1
#endif