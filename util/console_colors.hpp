#pragma once

extern bool ansi_color_supported;

void enable_console_ansi_color_codes ();

// Regular    : [31
// Bold       : [1;31
// Underlined : [4;31

#define ANSI_COLOR_BLACK         "\x1b[30m"
#define ANSI_COLOR_RED           "\x1b[31m"
#define ANSI_COLOR_GREEN         "\x1b[32m"
#define ANSI_COLOR_YELLOW        "\x1b[33m"
#define ANSI_COLOR_BLUE          "\x1b[34m"
#define ANSI_COLOR_MAGENTA       "\x1b[35m"
#define ANSI_COLOR_CYAN          "\x1b[36m"

#define ANSI_COLOR_BOLD_BLACK    "\x1b[1;30m" // really just grey
#define ANSI_COLOR_BOLD_RED      "\x1b[1;31m"
#define ANSI_COLOR_BOLD_GREEN    "\x1b[1;32m"
#define ANSI_COLOR_BOLD_YELLOW   "\x1b[1;33m"
#define ANSI_COLOR_BOLD_BLUE     "\x1b[1;34m"
#define ANSI_COLOR_BOLD_MAGENTA  "\x1b[1;35m"
#define ANSI_COLOR_BOLD_CYAN     "\x1b[1;36m"

#define ANSI_COLOR_RESET         "\x1b[0m"
