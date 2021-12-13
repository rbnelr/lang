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
	};

	~Value () {
		if (type == STR)
			free(str);
	}

	Value ()  {
		memset(this, 0, sizeof(Value));
	}
	Value (bool    b): type{BOOL},  b{b} {}
	Value (int64_t i): type{INT},   i{i} {}
	Value (double  f): type{FLT},   f{f} {}

	Value (Value const& v) = delete;
	//Value (Value const& v) {
	//	memset(this, 0, sizeof(Value));
	//	_copy(*this, v);
	//}
	Value (Value&& v) {
		memset(this, 0, sizeof(Value));
		_move(*this, v);
	}

	Value& operator= (Value const& v) = delete;
	//Value& operator= (Value const& v) {
	//	_copy(*this, v);
	//	return *this;
	//}
	Value& operator= (Value&& v) {
		_move(*this, v);
		return *this;
	}

	Value copy () const {
		Value val;
		_copy(val, *this);
		return val;
	}

	void set_str (std::string_view const& str) {
		this->str = (char*)malloc(str.size() + 1);
		memcpy(this->str, str.data(), str.size());
		this->str[str.size()] = '\0';
	}

	static void _copy (Value& l, Value const& r) {
		if (l.type == STR)
			free(l.str);
		l.type = NULL;

		if (r.type == STR) {
			l.type = r.type;
			l.set_str(r.str); // strlen + alloc + copy
		} else {
			memcpy(&l, &r, sizeof(Value));
		}
	}
	static void _move (Value& l, Value& r) {
		if (l.type == STR)
			free(l.str);
		l.type = NULL;

		memcpy(&l, &r, sizeof(Value));
		memset(&r, 0, sizeof(Value));
	}

	//Value (std::string_view&& str): type{STR} { set_str(str); }
};

void print_val (Value const& arg) {
	switch (arg.type) {
		case NULL:
			printf("null");
			break;
		case BOOL:
			printf("%s", arg.b ? "true":"false");
			break;
		case INT:
			printf("%" PRIi64, arg.i);
			break;
		case FLT:
			printf("%f", arg.f);
			break;
		case STR:
			printf("%s", arg.str);
			break;
		default:
			assert(false);
	}
}
