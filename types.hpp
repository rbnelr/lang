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

	Value () {
		type = NULL;
		u = {};
	}

	Value (bool    b): type{BOOL} { u.b = b; }
	Value (int64_t i): type{INT}  { u.i = i; }
	Value (double  f): type{FLT}  { u.f = f; }
};
#define NULLVAL (Value{})

void print_val (Value const& arg) {
	switch (arg.type) {
		case NULL:
			printf("null");
			break;
		case BOOL:
			printf("%s", arg.u.b ? "true":"false");
			break;
		case INT:
			printf("%" PRIi64, arg.u.i);
			break;
		case FLT:
			printf("%f", arg.u.f);
			break;
		case STR:
			printf("%s", arg.u.str);
			break;
		default:
			assert(false);
	}
}
