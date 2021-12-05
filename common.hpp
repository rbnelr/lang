#pragma once

#include "stdio.h"
#include "math.h"
#include "assert.h"

#include <string>
#include <vector>
#include <unordered_map>

#include "util/string.hpp"
#include "util/file_io.hpp"
#include "util/console_colors.hpp"

#include <exception>

#include "tracy.hpp"

using namespace kiss;
#define ARRLEN(arr) (sizeof(arr) / sizeof((arr)[0]))

#if   BUILD_DEBUG

#elif BUILD_TRACY

	//#define NDEBUG // no asserts

#elif BUILD_RELEASE

	//#define NDEBUG // no asserts

#endif