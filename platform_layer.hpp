#pragma once
#include "util/util.hpp"

enum MmapProtect {
	MMAP_READ  = 1,
	MMAP_WRITE = 2,
	MMAP_EXEC  = 4,
};
ENUM_BITFLAG_OPERATORS(MmapProtect)

struct MmapPageSize {
	unsigned page_size;
	unsigned alloc_granularity;
};

MmapPageSize mmap_get_page_size ();

void* mmap_pages_alloc (size_t size, size_t* alloc_size, MmapProtect prot);
void mmap_pages_free (void* ptr);
void mmap_pages_protect (void* ptr, size_t alloc_size, MmapProtect prot);

void mmap_invalidate_instruction_cache (void* ptr, size_t size);

inline MmapPageSize mmap_page_size = mmap_get_page_size();
