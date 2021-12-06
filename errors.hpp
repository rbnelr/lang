#pragma once
#include "common.hpp"

struct Exception {
	const char* errstr;
	const char* start;
	const char* cur;
	size_t      lineno;

	void print (char const* filename, std::vector<strview> const& lines) {
		assert(lineno < lines.size());
		auto& line = lines[lineno];

		size_t charno = start - line.data();

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);
		fprintf(stderr, "%s:%" PRIuMAX ":%" PRIuMAX ": error: %s.\n", filename, lineno+1, charno+1, errstr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);
		fwrite(line.data(), 1, line.size(), stderr);
		fputc('\n', stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);

		assert(start >= line.data());
		assert(cur > start);
		size_t begin = start - line.data();
		size_t len   = cur - start;

		for (size_t i=0; i<begin; ++i)
			fputc(' ', stderr);

		fputs("^", stderr);
		for (size_t i=1; i<len; ++i)
			fputc('~', stderr);

		fputs("\n", stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};
