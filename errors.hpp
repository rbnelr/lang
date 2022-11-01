#pragma once
#include "common.hpp"
#include "frontend/lexer.hpp"

inline constexpr char const* CONCOL_ERR      = "\x1b[0;1;31m";
inline constexpr char const* CONCOL_ERR_SRC  = "\x1b[0;1;37m";
inline constexpr char const* CONCOL_NOTE     = "\x1b[0m";
inline constexpr char const* CONCOL_NOTE_SRC = "\x1b[0;1;30m";

struct ErrorSource {
	const char*  errtype;
	SourceRange  src;
	std::string  msg;

	//ErrorSource (const char* errtype, SourceRange src, std::string&& msg): errtype{errtype}, src{src}, msg{msg} {}
	
	static inline int tab_spaces = 4;

	void print (char const* filename, const char* col1, const char* col2) {
		constexpr int CONSOLE_WIDTH = 80;
		constexpr int MAX_LINE_LEN  = CONSOLE_WIDTH - 2; // need at max 4 additional chars to print ^... for src ranges on end of line and extending further

		char source[CONSOLE_WIDTH+1] = {};
		char arrow [CONSOLE_WIDTH+1] = {};

		if (src.start == nullptr) {
			strncpy(source, "<source null>", CONSOLE_WIDTH);
		}
		else {
			assert(src.length > 0);
			assert(src.start_lineno > 0);

			// TODO: on lines longer than some amount (say 80 chars) don't print the entire line
			// but instead print 80 chars of the line starting, such that the token is in the middle
			// tokens longer than 80 chars are also cut off such that we only see the start of them
			// eg:
			// ..."my very long string literal blahhhhhhhhh blahhhhhhhhh foo baaaaaaaaaaaaaaaaaaaar...
			//    ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~v
			
			char const* str = src.start - src.start_charno;
			assert(src.start >= str);

			auto end_of_line = [] (char c) {
				return c=='\0' || c=='\n' || c=='\r';
			};

			int in  = 0;
			int out = 0;

			int range_start = (int) src.start_charno;
			int range_end   = (int)(src.start_charno + src.length);
			int range_arrow = (int)(src.start_charno); //  + src.arrow

			// fill up source with the source line truncated to CONSOLE_WIDTH characters and with tabs turned into spaces
			// fill up arrow with the ^~~~ indicator of the src range, where it actually corresponds with the source in terms of tabs
			while (!end_of_line(str[in]) && out < MAX_LINE_LEN) {
				assert(out >= in);
				
				char arrowchar;
				if (in < range_start || in >= range_end) arrowchar = ' ';
				else if (in == range_arrow)              arrowchar = '^';
				else                                     arrowchar = '~';

				if (str[in] != '\t') {
					source[out]  = str[in];
					arrow[out++] = arrowchar;
				}
				else {
					do {
						source[out]  = ' '; // out > MAX_LINE_LEN ? '.' : 
						arrow[out++] = arrowchar;
					} while (out % tab_spaces && out < MAX_LINE_LEN);
				}
				in++;
			}

			{
				if (in == range_arrow) {
					// handle src range starting on end of line
					arrow[out++] = '^';
					in++;
				}
				
				// handle src going past end of line
				if (in < range_end) {
					// src range end is after end of line
					arrow[out++] = 'v';
				}
			}

			assert(out <= CONSOLE_WIDTH);
		}

		if (ansi_color_supported) fputs(col1, stderr);
		fprintf(stderr, "%s:%u:%u: %s: %s.\n", filename, src.start_lineno, src.start_charno, errtype, msg.c_str());

		bool prefix_lineno = true;

		int lineno_w = 0;
		if (prefix_lineno) {
			if (ansi_color_supported) fputs(ANSI_COLOR_BOLD_BLACK, stderr);
			lineno_w = fprintf(stderr, "%3d | ", src.start_lineno);
		}

		if (ansi_color_supported) fputs(col2, stderr);
		fprintf(stderr, "%s\n", source);

		if (ansi_color_supported) fputs(col1, stderr);
		for (int i=0; i<lineno_w; ++i) fputs(" ", stderr);
		fprintf(stderr, "%s\n", arrow);

		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};

struct CompilerExcept {
	ErrorSource              err;
	smallvec<ErrorSource, 8> notes;

	CompilerExcept (ErrorSource err, std::initializer_list<ErrorSource> notes = {}): err{err}, notes{notes} {}

	void print (char const* filename) {
		err.print(filename, CONCOL_ERR, CONCOL_ERR_SRC);

		for (auto& note : notes)
			note.print(filename, CONCOL_NOTE, CONCOL_NOTE_SRC);
	}
};

[[noreturn]] inline void _ERROR (const char* errtype, SourceRange const& src, std::string&& msg) {
	throw CompilerExcept{{ errtype, src, std::move(msg) }};
}

template <typename... Args>
[[noreturn]] _NOINLINE inline void SYNTAX_ERROR (SourceRange const& src, const char* format, ...) {
	va_list vl;
	va_start(vl, format);
	auto str = vprints(format, vl);
	va_end(vl);

	_ERROR("syntax error", src, std::move(str));
}
template <typename... Args>
[[noreturn]] _NOINLINE inline void ERROR (SourceRange const& src, const char* format, ...) {
	va_list vl;
	va_start(vl, format);
	auto str = vprints(format, vl);
	va_end(vl);

	_ERROR("error", src, std::move(str));
}

struct RuntimeExcept {
	std::string errstr;

	RuntimeExcept (const char* str): errstr{str} {}
	RuntimeExcept (std::string&& str): errstr{std::move(str)} {}

	void print () {
		if (ansi_color_supported) fputs(CONCOL_ERR, stderr);
		fprintf(stderr, "%s.\n", errstr.c_str());
		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stderr);

		fflush(stderr);
	}
};
