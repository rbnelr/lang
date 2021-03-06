#pragma once
#include "common.hpp"
#include "line_map.hpp"

struct CompilerExcept {
	const char* errstr;

	source_range tok;

	static inline int tab_spaces = 4;

	void print (char const* filename, SourceLines const& lines) {
		assert(tok.end > tok.start);

		size_t start_lineno = lines.find_lineno(tok.start);
		auto line_str = lines.get_line_text(start_lineno);

		auto print_line = [&] () {
			for (char c : line_str) {
				if (c != '\t') fputc(c, stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(' ', stderr);
			}
		};
		auto print_line_range = [&] (size_t begin, size_t end) {
			auto sz = line_str.size();
			for (size_t i=0; i<begin; ++i) {
				if (i >= sz || line_str[i] != '\t')   fputc(' ', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(' ', stderr);
			}
			{ size_t i=begin;
				if (i >= sz || line_str[i] != '\t')   fputc('^', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc(j == 0 ? '^':'~', stderr);
			}
			for (size_t i=begin+1; i<end; ++i) {
				if (i >= sz || line_str[i] != '\t')   fputc('~', stderr);
				else for (int j=0; j<tab_spaces; ++j) fputc('~', stderr);
			}
		};

		size_t charno = tok.start - line_str.data();

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);
		fprintf(stderr, "%s:%" PRIuMAX ":%" PRIuMAX ": %s.\n", filename, start_lineno+1, charno+1, errstr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);
		
		print_line();
		fputc('\n', stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);

		assert(tok.start >= line_str.data());

		// source.start and source.end could be in a bit after the line str due to the newline being excluded
		// this is handled in print_line_range though
		//assert(source.end   <= line_str.data() + line_str.size());

		print_line_range(tok.start - line_str.data(), tok.end - line_str.data());
		fputs("\n", stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};

struct RuntimeExcept {
	const char* errstr;

	void print () {
		if (ansi_color_supported) fputs(ANSI_COLOR_RED, stderr);
		fprintf(stderr, "%s.\n", errstr);
		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};
