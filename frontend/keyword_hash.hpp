/* C++ code produced by gperf version 3.0.1 */
/* Command-line: ./bin/gperf.exe -L C++ lang.txt  */
/* Computed positions: -k'1,3' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

bool keyword_hash_compare (const char* a, const char* b, size_t len) {
	const char* end = a + len;
	while (a < end) {
		if (*a++ != *b++) return false;
	}
	return true;
}

size_t keyword_hash (const char* str, size_t len) {
	static constexpr char asso_values[] = {
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 15,  5,
		5,  0,  5, 20, 25,  0, 25, 25,  0, 25,
		5, 25, 25, 25,  0,  5,  0, 15, 10,  0,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25
	};
	size_t hval = len;

	switch (hval) {
		default:
			hval += asso_values[(unsigned char)str[2]];
			/*FALLTHROUGH*/
		case 2:
		case 1:
			hval += asso_values[(unsigned char)str[0]];
			break;
	}
	return hval;
}

TokenType get_keyword (const char* str, size_t len) {
	//static const char * wordlist[] = {
	//		"", "",
	//		"if",
	//		"let",
	//		"elif",
	//		"while",
	//		"return",
	//		"do",
	//		"for",
	//		"else",
	//		"false",
	//		"struct",
	//		"",
	//		"var",
	//		"func",
	//		"const",
	//		"", "",
	//		"continue",
	//		"true",
	//		"break",
	//		"", "", "",
	//		"goto"
	//	};

	static constexpr _Keyword wordlist[] = {
		{"" },
		{"" },
		{ "if"        , T_IF             },
		{ "let"       , T_LET            },
		{ "elif"      , T_ELIF           },
		{ "while"     , T_WHILE          },
		{ "return"    , T_RETURN         },
		{ "do"        , T_DO             },
		{ "for"       , T_FOR            },
		{ "else"      , T_ELSE           },
		{ "false"     , T_LITERAL_BOOL   },
		{ "struct"    , T_STRUCT         },
		{"" },
		{ "var"       , T_VAR            },
		{ "func"      , T_FUNC           },
		{ "const"     , T_CONST          },
		{"" },
		{"" },
		{ "continue"  , T_CONTINUE       },
		{ "true"      , T_LITERAL_BOOL   },
		{ "break"     , T_BREAK          },
		{"" },
		{"" },
		{"" },
		{ "goto"      , T_GOTO           },
	};
	
	constexpr size_t TOTAL_KEYWORDS  = 17;
	constexpr size_t MIN_WORD_LENGTH = 2;
	constexpr size_t MAX_WORD_LENGTH = 8;
	constexpr size_t MIN_HASH_VALUE  = 2;
	constexpr size_t MAX_HASH_VALUE  = 24;
	/* maximum key range = 23, duplicates = 0 */

	if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH) {
		size_t key = keyword_hash(str, len);

		if (key <= MAX_HASH_VALUE && key >= 0) {
			auto& word = wordlist[key];

			if (word.str.size() == len && keyword_hash_compare(str, word.str.data(), len)) {
				return word.tok;
			}
		}
	}
	return T_IDENTIFIER;
}
