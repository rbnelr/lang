#pragma once
#include "common.hpp"

#undef VOID
#undef BOOL
#undef INT
#undef FLT
#undef STR

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

inline void dbg_print (Type type, Value const& val) {
	switch (type) {
		case BOOL: case INT:
			printf("%lli", val.i);
			break;
		case FLT:
			printf("%f", val.f);
			break;
		case STR: {
			auto str = escape_string_capped(val.str);
			printf("\"%s\"", str.c_str());
		} break;
	}
}
