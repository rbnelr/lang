#pragma once
#include "llvm_pch.hpp"

#include "windows.h"

void* mmap_pages_alloc (size_t size, size_t* alloc_size) {
	constexpr size_t page_sz = 0x1000;
	*alloc_size = (size + page_sz-1) & ~(page_sz-1); // round up size
				
	void* ptr = VirtualAlloc(NULL, *alloc_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	//auto err = GetLastError();
	assert(ptr != nullptr);

	assert(*alloc_size > size);
#ifndef NDEBUG
	// mark actually allocated section memory as UNINIT
	memset(ptr,               _DBG_MAGIC_UNINIT, size);
	// mark not actually requested but allocated section memory as NONALLOC
	// NOTE: This region should never be accessed, since it's only allocated by chance, yet getSectionContent return part of this!!
	memset((char*)ptr + size, _DBG_MAGIC_NONALLOC, *alloc_size - size);
#endif

	return ptr;
}
void mmap_pages_free (void* ptr) {
	auto res = VirtualFree(ptr, 0, MEM_RELEASE);
	//auto err = GetLastError();
	assert(res != 0);
}

void mmap_pages_protect (void* ptr, size_t alloc_size, llvm::sys::Memory::ProtectionFlags flags) {
	DWORD oldProtect;
	DWORD newProtect = 0;

	using namespace llvm::sys;
	switch (flags) {
		case Memory::MF_READ:
			newProtect = PAGE_READONLY;
			break;
		case Memory::MF_READ | Memory::MF_WRITE:
			newProtect = PAGE_READWRITE;
			break;
		case Memory::MF_READ | Memory::MF_EXEC:
			newProtect = PAGE_EXECUTE_READ;
			break;
		default:
			assert(false);
	}

	auto res = VirtualProtect(ptr, alloc_size, newProtect, &oldProtect);
	//auto err = GetLastError();
	assert(res != 0);
}
