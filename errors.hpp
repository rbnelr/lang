#pragma once
#include "common.hpp"
#include "line_map.hpp"

inline constexpr char const* CONCOL_ERR      = "\x1b[0;1;31m";
inline constexpr char const* CONCOL_ERR_SRC  = "\x1b[0;1;37m";
inline constexpr char const* CONCOL_NOTE     = "\x1b[0m";
inline constexpr char const* CONCOL_NOTE_SRC = "\x1b[0;1;30m";

struct ErrorSource {
	const char*  errtype;
	source_range src;
	std::string  msg;
	
	static inline int tab_spaces = 4;

	void print (char const* filename, SourceLines const& lines, const char* col1, const char* col2) {
		assert(src.end > src.start);

		size_t start_lineno = lines.find_lineno(src.start);
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

		size_t charno = src.start - line_str.data();

		if (ansi_color_supported) fputs(col1, stderr);
		fprintf(stderr, "%s:%" PRIuMAX ":%" PRIuMAX ": %s: %s.\n", filename, start_lineno+1, charno, errtype, msg.c_str());

		if (ansi_color_supported) fputs(col2, stderr);
		
		print_line();
		fputc('\n', stderr);

		if (ansi_color_supported) fputs(col1, stderr);

		assert(src.start >= line_str.data());

		// source.start and source.end could be in a bit after the line str due to the newline being excluded
		// this is handled in print_line_range though
		//assert(source.end   <= line_str.data() + line_str.size());

		print_line_range(src.start - line_str.data(), src.end - line_str.data());
		fputs("\n", stderr);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};

struct CompilerExcept {
	ErrorSource              err;
	smallvec<ErrorSource, 8> notes;

	void print (char const* filename, SourceLines const& lines) {
		err.print(filename, lines, CONCOL_ERR, CONCOL_ERR_SRC);

		for (auto& note : notes)
			note.print(filename, lines, CONCOL_NOTE, CONCOL_NOTE_SRC);
	}
};

[[noreturn]] inline void _ERROR (const char* errtype, source_range const& src, const char* format, const std::format_args _Args) {
	throw CompilerExcept{{ errtype, src, std::vformat(format, _Args) }};
}

template <typename... Args>
[[noreturn]] inline void SYNTAX_ERROR (source_range const& src, const char* format, Args... args) {
	_ERROR("syntax error", src, format, std::make_format_args(args...));
}
template <typename... Args>
[[noreturn]] inline void ERROR (source_range const& src, const char* format, Args... args) {
	_ERROR("error", src, format, std::make_format_args(args...));
}

struct RuntimeExcept {
	const char* errstr;

	void print () {
		if (ansi_color_supported) fputs(CONCOL_ERR, stderr);
		fprintf(stderr, "%s.\n", errstr);
		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};
