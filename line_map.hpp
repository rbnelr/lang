#pragma once
#include "common.hpp"

// could use a uint64_t/size_t that's relative to the start of the source text
// but simply using a const char* allows me to directly see the text in the debugger
typedef const char* source_loc_t;

struct source_range {
	source_loc_t start; // first char
	source_loc_t end;   // one past last char

	std::string_view text () {
		return std::string_view(start, (size_t)(end - start));
	}
};

struct SourceLines {
	std::vector<source_range> lines; // each line with the newline character sequence excluded

	size_t find_lineno (source_loc_t loc) const {
		// binary search line that loc is in
		// (if in the newline that was excluded still count it as the line the newline belongs to)
		auto it = std::lower_bound(lines.begin(), lines.end(), loc, [] (source_range const& line, source_loc_t loc) {
			return loc < line.start;
		});
		return (size_t)(it - lines.begin());
	}
};
