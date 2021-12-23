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


// just for printing string to the console which might contain newlines or nulls
std::string escape_string_capped (std::string_view const& str, size_t max_len=(size_t)-1) {
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

void dbg_print (Type type, Value const& val) {
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
