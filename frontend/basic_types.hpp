#pragma once
#include "common.hpp"

enum TypeClass : uint8_t {
	TY_BOOL,
	TY_INT,
	TY_FLT,
	TY_STR,
	
	TY_FUNC,
	TY_STRUCT,
};

union Value {
	bool          b;
	int64_t       i;
	double        f;
	const char*   str; // non-owning
};

inline void dbg_print (TypeClass tclass, Value const& val) {
	switch (tclass) {
		case TY_BOOL: case TY_INT:
			printf("%lli", val.i);
			break;
		case TY_FLT:
			printf("%f", val.f);
			break;
		case TY_STR: {
			auto str = escape_string_capped(val.str);
			printf("\"%s\"", str.c_str());
		} break;
	}
}