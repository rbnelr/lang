#pragma once
#include "stdarg.h"
#include <string>
#include <string_view>
//#include <regex>
#include <format>

namespace kiss {
	// Printf that appends to a std::string
	void vprints (std::string* s, char const* format, va_list vl);

	// Printf that appends to a std::string
	void prints (std::string* s, char const* format, ...);

	// Printf that outputs to a std::string
	std::string prints (char const* format, ...);

	bool starts_with (std::string_view const& str, std::string_view const& substr);

	// remove whitespace at front and back
	std::string_view trim (std::string_view sv);

	inline std::string operator+ (std::string_view const& l, std::string_view const& r) {
		std::string s;
		s.reserve(l.size() + r.size()); // null terminator is implicit see https://stackoverflow.com/questions/30111288/stdstringreserve-and-end-of-string-0
		s.insert(0, l);
		s.insert(l.size(), r);
		return s;
	}

	//template <typename FUNC>
	//std::string regex_replace (std::string const& str, std::regex const& re, FUNC for_match) {
	//	size_t cur = 0;
	//
	//	std::string out;
	//
	//	for (auto it = std::sregex_iterator(str.begin(), str.end(), re); it != std::sregex_iterator(); ++it) {
	//		auto match = *it;
	//
	//		out += str.substr(cur, match.position());
	//		cur = match.position() + match.length();
	//
	//		out += for_match(match);
	//	}
	//
	//	out += str.substr(cur, str.size());
	//
	//	return out;
	//}

//#ifdef _WIN32
//	// Prefer this being here rather than platform independed string.hpp
//	std::basic_string<wchar_t> utf8_to_wchar (std::string_view utf8);
//	std::string wchar_to_utf8 (std::basic_string_view<wchar_t> wchar);
//#endif

	inline bool is_utf8_code (char c) {
		return (c & 0x80) != 0;
	}
	inline bool is_utf8_code (char const* str) {
		return (str[0] & 0x80) != 0;
	}

	inline int utf8_decode_single (char const* str, int* out_codepoint) { // TODO: test this!
		if ((str[0] & 0x80) == 0) {
			*out_codepoint = str[0]; // return ascii as int
			return 1;
		}

		int count;
		if (((int)str[0] & 0xE0) == 0xC0) {
			// two-byte encoding
			*out_codepoint = (((int)str[0] & 0x1F) << 6) | ((int)str[1] & 0x3F);
			count = 2;
		} else if (((int)str[0] & 0xF0) == 0xE0) {
			// three-byte encoding
			*out_codepoint = (((int)str[0] & 0x0F) << 6) | (((int)str[1] & 0x3F) << 12) | ((int)str[2] & 0x3F);
			count = 3;
		} else if (((int)str[0] & 0xF8) == 0xF0) {
			// four-byte encoding
			*out_codepoint = (((int)str[0] & 0x07) << 6) | (((int)str[1] & 0x3F) << 12) | (((int)str[2] & 0x3F) << 18) | ((int)str[3] & 0x3F);
			count = 4;
		} else {
			//assert(false);
			*out_codepoint = 0;
			count = 1;
		}
		return count;
	}
	inline int utf8_decode_single (char const*& str) {
		int codepoint;
		str += utf8_decode_single(str, &codepoint);
		return codepoint;
	}
	inline int _utf8_decode_single (char const* str) {
		int codepoint;
		utf8_decode_single(str, &codepoint);
		return codepoint;
	}
}
