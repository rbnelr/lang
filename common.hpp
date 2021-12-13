#pragma once

#include "stdio.h"
#include "math.h"
#include "assert.h"

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>

#include "util/string.hpp"
#include "util/file_io.hpp"
#include "util/console_colors.hpp"
#include "util/timer.hpp"

#include <exception>

#include "inttypes.h" // for PRIuMAX for size_t printf'ing

#include "tracy.hpp"

#define ARRLEN(arr) (sizeof(arr) / sizeof((arr)[0]))

using namespace kiss;

typedef std::string_view strview;
