#pragma once
#include "common.hpp"

// could use a uint64_t/size_t that's relative to the start of the source text
// but simply using a const char* allows me to directly see the text in the debugger
typedef const char* source_loc_t;

struct source_range {
	source_loc_t start; // first char
	source_loc_t end;   // one past last char

	source_range () {}
	constexpr source_range (source_loc_t start, source_loc_t end): start{start}, end{end} {}
	constexpr explicit source_range (std::string_view sv): start{sv.data()}, end{sv.data() + sv.size()} {}

	constexpr std::string_view text () const {
		return std::string_view(start, (size_t)(end - start));
	}
};

struct SourceLines {
	std::vector<source_loc_t> lines; // each line with the newline character sequence excluded

	void parse_lines (const char* src) {
		ZoneScoped;

		lines.reserve(1024*8);
		const char* cur = src;
		const char* cur_line = src;

		// add first line
		lines.emplace_back(src);

		while (*cur != '\0') {
			if (*cur == '\n' || *cur == '\r') {
				// newline found
				char c = *cur++;

				// this code should even handle files with inconsistent unix vs windows newlines reasonably
				// "\n" "\r" "\n\r" "\r\n" each count as one newline while "\n\n" "\r\r" count as two
				if ((*cur == '\n' || *cur == '\r') && c != *cur)
					cur++;

				// add next line
				lines.emplace_back(cur);
			}
			else {
				cur++;
			}
		}

		// add dummy line after EOF on one past the EOF character
		// TODO: is this safe? I changed this from the dummy line being _on_ the EOF
		// since my binary search below had a bug in the case of the source loc being _on_ the EOF as well
		lines.emplace_back(cur + 1);
	}

	std::string_view get_line_text (size_t lineno) const {
		assert(lineno+1 < lines.size()); // +1 to exclude dummy line

		//const char* end = strtok((char*)lines[lineno], "\n\r");

		const char* start = lines[lineno];
		const char* end = start;
		while (*end != '\r' && *end != '\n' && *end != '\0')
			end++;

		return std::string_view(start, (size_t)(end - start) );
	}

	size_t find_lineno (source_loc_t loc) const {
		ZoneScoped;
		// binary search line that loc is in

		size_t lo = 0;
		size_t hi = lines.size()-1; // exclude dummy line

		for (;;) {
			assert(lo < hi);

			size_t lineno = lo + (hi-lo) / 2;

			auto line_start = lines[lineno];
			auto line_end   = lines[lineno+1];

			// loc after line end
			if (loc < line_start)
				hi = lineno;
			// loc before line start
			else if (loc >= line_end)
				lo = lineno+1;
			// line found
			else
				return lineno;
		}
	}
};
