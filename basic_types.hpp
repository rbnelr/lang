#pragma once
#include "common.hpp"

#undef NULL

enum Type : uint32_t {
	VOID=0,
	BOOL,
	INT,
	FLT,
	STR,
};
inline constexpr const char* Type_str[] = {
	"VOID",
	"BOOL",
	"INT",
	"FLT",
	"STR",
};

union Value {
	bool          b;
	int64_t       i;
	double        f;
	const char*   str; // non-owning
};
