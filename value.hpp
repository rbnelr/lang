#pragma once
#include "common.hpp"

#undef NULL

enum Type : uint32_t {
	NULL=0,
	BOOL,
	INT,
	FLT,
	STR,
};

struct Value {
	Type type;
	union {
		bool          b;
		int64_t       i;
		double        f;
		const char*   str; // non-owning
	} u;

	constexpr Value (): type{NULL}, u{} {}

	Value (bool    b): type{BOOL} { u.b = b; }
	Value (int64_t i): type{INT}  { u.i = i; }
	Value (double  f): type{FLT}  { u.f = f; }
};

static inline constexpr Value NULLVAL = Value{};
