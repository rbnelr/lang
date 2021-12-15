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
		bool    b;
		int64_t i;
		double  f;
		char*   str;
	} u;

	_FORCEINLINE ~Value () {
		if (type == STR)
			free(u.str);
	}

	_FORCEINLINE Value () {
		type = NULL;
		u = {};
	}

	_FORCEINLINE Value (bool    b): type{BOOL} { u.b = b; }
	_FORCEINLINE Value (int64_t i): type{INT}  { u.i = i; }
	_FORCEINLINE Value (double  f): type{FLT}  { u.f = f; }

	// automatic move via assignment (only for convinience, and in case containers need to use it)
	// only allowed for null values since compiler is not smart enough to optimize away the free even if it's never needed
	// (frees appear all over the place)
	// in the few places where we need to actually overwrite a value we use assign() instead
	_FORCEINLINE Value& operator= (Value&& r) {
		assert(type == NULL);
		memcpy(this, &r, sizeof(Value));
		memset(&r, 0, sizeof(Value));
		return *this;
	}
	// allow move ctor to enable use in containers
	_FORCEINLINE Value (Value&& r) noexcept {
		memcpy(this, &r, sizeof(Value));
		memset(&r, 0, sizeof(Value));
	}

	_FORCEINLINE void set_null () {
		if (type == STR)
			free(u.str);
		memset(this, 0, sizeof(Value));
	}

	// no automatic copy for this class
	Value (Value const& v) = delete;
	Value& operator= (Value const& v) = delete;
	// manaul copy where needed
	_FORCEINLINE Value copy () const noexcept {
		Value val;
		if (type != STR) {
			memcpy(&val, this, sizeof(Value));
			return val;
		}
		val.set_str(u.str); // strlen + alloc + copy
		return val;
	}

	void set_str (std::string_view const& str) noexcept {
		type = STR;
		u.str = (char*)malloc(str.size() + 1);
		memcpy(u.str, str.data(), str.size());
		u.str[str.size()] = '\0';
	}
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
