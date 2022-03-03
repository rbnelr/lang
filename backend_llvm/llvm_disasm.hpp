#pragma once
#include "llvm_pch.hpp"

struct LoadedSections {
	struct Segment {
		MmapProtect          type;
		void*                addr;
		size_t               size;
		size_t               alloc_size;

		const char* get_name () {
			switch (type) {
				case MMAP_READ | MMAP_EXEC  : return "READ_EXEC";
				case MMAP_READ              : return "READ_ONLY";
				case MMAP_READ | MMAP_WRITE : return "READ_WRITE";
				INVALID_DEFAULT;
			}
		}
	};
	struct Symbol {
		void*       addr;
		std::string name;
	};

	std::vector<Segment> segments;
	std::vector<Symbol>  symbols;

	void sort_symbols () {
		std::stable_sort(symbols.begin(), symbols.end(), [] (Symbol const& l, Symbol const& r) {
			return std::less<uintptr_t>()((uintptr_t)l.addr, (uintptr_t)r.addr);
		});
	}
};

void print_llvm_disasm (llvm::Triple const& TT, LoadedSections& sections);
