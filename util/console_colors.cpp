#include "console_colors.hpp"

bool ansi_color_supported = false;

#ifdef _WIN32
	#include "windows.h"

	bool _enable_console_ansi_color_codes (DWORD nStdHandle) {
		auto conh = GetStdHandle(nStdHandle);

		DWORD mode = 0;

		if (!GetConsoleMode(conh, &mode))
			return false;

		mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
		if (!SetConsoleMode(conh, mode))
			return false;

		return true;
	}

	void enable_console_ansi_color_codes () {
		auto res0 = _enable_console_ansi_color_codes(STD_OUTPUT_HANDLE);
		auto res1 = _enable_console_ansi_color_codes(STD_ERROR_HANDLE);

		ansi_color_supported = res0 != 0 && res1 != 0;
	}
#else
	void enable_console_ansi_color_codes () {

	}
#endif
