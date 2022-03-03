#include "platform_layer.hpp"
#include "assert.h"
#include "windows.h"

MmapPageSize mmap_get_page_size () {
	SYSTEM_INFO info = {};
    GetSystemInfo(&info);
	
	MmapPageSize sz = {};
	sz.page_size         = (unsigned)info.dwPageSize;
	sz.alloc_granularity = (unsigned)info.dwAllocationGranularity;
	return sz;
}

DWORD map_protect (MmapProtect prot) {
	switch (prot) {
		case MMAP_READ:              return PAGE_READONLY;
		case MMAP_READ | MMAP_WRITE: return PAGE_READWRITE;
		case MMAP_READ | MMAP_EXEC:  return PAGE_EXECUTE_READ;
		INVALID_DEFAULT;
	}
}

void* mmap_pages_alloc (size_t size, size_t* alloc_size, MmapProtect prot) {
	*alloc_size = align_up(size, mmap_page_size.page_size);
	
	auto win32_prot = map_protect(prot);

	void* ptr = VirtualAlloc(NULL, *alloc_size, MEM_COMMIT | MEM_RESERVE, win32_prot);
	//auto err = GetLastError();
	assert(ptr != nullptr);

	assert(*alloc_size > size);
#ifndef NDEBUG
	// mark actually allocated section memory as UNINIT
	memset(ptr,               _DBG_MAGIC_UNINIT, size);
	// mark not actually requested but allocated section memory as NONALLOC
	memset((char*)ptr + size, _DBG_MAGIC_NONALLOC, *alloc_size - size);
#endif

	return ptr;
}
void mmap_pages_protect (void* ptr, size_t alloc_size, MmapProtect prot) {
	DWORD new_prot = map_protect(prot);
	DWORD old_prot;
	
	auto res = VirtualProtect(ptr, alloc_size, new_prot, &old_prot);
	//auto err = GetLastError();
	assert(res != 0);
}
void mmap_pages_free (void* ptr) {
	auto res = VirtualFree(ptr, 0, MEM_RELEASE);
	//auto err = GetLastError();
	assert(res != 0);
}

void mmap_invalidate_instruction_cache (void* ptr, size_t size) {
	auto pid = GetCurrentProcess();
	FlushInstructionCache(ptr, ptr, size);
}
