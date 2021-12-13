#pragma once
#include "common.hpp"

#undef NULL

enum Type {
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

	~Value () {
		if (type == STR)
			free(u.str);
	}

	Value () {
		type = NULL;
		u = {};
	}

	Value (bool    b): type{BOOL} { u.b = b; }
	Value (int64_t i): type{INT}  { u.i = i; }
	Value (double  f): type{FLT}  { u.f = f; }

	Value (Value const& v) = delete;
	Value (Value&& v) {
		type = NULL;
		u = {};
		_move(*this, v);
	}

	Value& operator= (Value const& v) = delete;
	Value& operator= (Value&& v) {
		_move(*this, v);
		return *this;
	}

	void set_null () {
		if (type == STR)
			free(u.str);
		type = NULL;
		u = {};
	}

	static void _move (Value& l, Value& r) {
		//if (l.type == STR)
		//	free(l.str);
		//l.type = NULL;

		// Only allow moving into structs that are NULL to avoid free calls all over the place
		// instead manually set_null() only in the places where you overwrite values (only one place in the interpreter)
		assert(l.type == NULL);
		
		l.type = r.type;
		l.u = r.u;

		r.type = NULL;
		r.u = {};
	}

	Value copy () const {
		Value val;
		val.type = type;
		if (type != STR) {
			val.u = u;
			return val;
		}
		val.set_str(u.str); // strlen + alloc + copy
		return val;
	}

	void set_str (std::string_view const& str) {
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
