#pragma once
#include "common.hpp"
#include "line_map.hpp"

struct Exception {
	const char* errstr;
	const char* start;
	const char* cur;

	static inline int tab_spaces = 4;

	void print (char const* filename, SourceLines const& lines) {
		size_t start_lineno = lines.find_lineno(start);
		auto& line = lines.lines[start_lineno];
		auto line_str = std::string_view(line.start, (size_t)(line.end - line.start));

		auto print_line = [&] () {
			for (char c : line_str) {
				if (c != '\t') fputc(c, stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(' ', stderr);
			}
		};
		auto print_line_range = [&] (size_t begin, size_t end) {
			for (size_t i=0; i<begin; ++i) {
				if (line_str[i] != '\t')              fputc(' ', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(' ', stderr);
			}
			{ size_t i=begin;
				if (line_str[i] != '\t')              fputc('^', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(j == 0 ? '^':'~', stderr);
			}
			for (size_t i=begin+1; i<end; ++i) {
				if (line_str[i] != '\t')              fputc('~', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc('~', stderr);
			}
		};

		size_t charno = start - line.start;

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);
		fprintf(stderr, "%s:%" PRIuMAX ":%" PRIuMAX ": error: %s.\n", filename, start_lineno+1, charno+1, errstr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);
		
		print_line();
		fputc('\n', stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);

		assert(start >= line.start);
		assert(cur > start);
		size_t begin = start - line.start;
		size_t end   = (size_t)std::min(cur - line.start, line.end - line.start);

		print_line_range(begin, end);
		fputs("\n", stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};
