#pragma once
#include "common.hpp"

struct Exception {
	const char* errstr;
	const char* start;
	const char* cur;
	size_t      lineno;

	static inline int tab_spaces = 4;

	void print (char const* filename, std::vector<strview> const& lines) {
		assert(lineno < lines.size());
		auto& line = lines[lineno];

		auto print_line = [&] () {
			for (char c : line) {
				if (c != '\t') fputc(c, stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(' ', stderr);
			}
		};
		auto print_line_range = [&] (size_t begin, size_t end) {
			for (size_t i=0; i<begin; ++i) {
				if (line[i] != '\t')                  fputc(' ', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(' ', stderr);
			}
			{ size_t i=begin;
				if (line[i] != '\t')                  fputc('^', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(j == 0 ? '^':'~', stderr);
			}
			for (size_t i=begin+1; i<end; ++i) {
				if (line[i] != '\t')                  fputc('~', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc('~', stderr);
			}
		};

		size_t charno = start - line.data();

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);
		fprintf(stderr, "%s:%" PRIuMAX ":%" PRIuMAX ": error: %s.\n", filename, lineno+1, charno+1, errstr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);
		
		print_line();
		fputc('\n', stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);

		assert(start >= line.data());
		assert(cur > start);
		size_t begin = start - line.data();
		size_t end   = std::min((size_t)(cur - line.data()), line.size());

		print_line_range(begin, end);
		fputs("\n", stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};
