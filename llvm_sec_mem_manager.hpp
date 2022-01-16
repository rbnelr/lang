#pragma once
#include "llvm_pch.hpp"
#include "llvm_backend.hpp"

//// Memory manager that handles code and code-data memory pages (memory for sections bascially)

// since the llvm::sys mmap page allocation functions round up your allocation sizes for seemingly no reason,
// wrap unix mmap and win32 VirtualAlloc manually
void* mmap_pages_alloc (size_t size, size_t* alloc_size);
void mmap_pages_free (void* ptr);
void mmap_pages_protect (void* ptr, size_t alloc_size, llvm::sys::Memory::ProtectionFlags flags);

// Use my own class here instead of llvm::SectionMemoryManager to allow me to know about all memory used and disassemble it properly
class SectionMemoryManager : public llvm::RTDyldMemoryManager {
public:

	struct Allocator {
		// Just doing the simplest way of allocation section I can think of
		// which is to allocate one series of pages per section
		// this is also the most flexible approach, since there will never be any fragmentation if pages are freed in random order
		// though the wasted memory at the end of pages could be considered fragmentation
		// 
		// TODO: In practice we want to avoid wasting memory if a large number of sections are allocated
		// The proper way to do this while still allowing for unloading of modules would be to merge section allocations where appropriate
		// but then this using this interface is likely the wrong approach
		// I'd keep allocations for seperate modules seperated to allow for unloading of modules without memory fragmentation,
		// group the sections of each modules into the 3 Purposes and then allocate 1 fixed-sized region for each
		// Thus every is simple and we have a worst case of ~3x the OS page size per module, which seems totally fine unless you have tons of modules for some reason
		// But for this approach this interface sucks since we would really want those sections to already be sorted by the user of this class

		struct Section {
			void*                  ptr;
			size_t                 sec_size;
			size_t                 alloc_size;

			// TODO: consider alignment here, for now I'm going to assume sections are never aligned to more than the alignment returned by this
			void alloc (size_t size) {
				ptr = mmap_pages_alloc(size, &alloc_size);
				sec_size = size;
			}
			~Section () {
				mmap_pages_free(ptr);
			}

			void protect (llvm::sys::Memory::ProtectionFlags flags) {
				mmap_pages_protect(ptr, alloc_size, flags);
			}
		};
			
		llvm::SmallVector<Section, 8> sections;
	};

		
	enum Purpose {
		Code = 0,
		ROData,
		RWData,
	};
	Allocator allocators[3];
		
	SectionMemoryManager () {};

	SectionMemoryManager(const SectionMemoryManager&) = delete;
	void operator=(const SectionMemoryManager&) = delete;

	~SectionMemoryManager() override {}

	uint8_t* allocateCodeSection (uintptr_t Size, unsigned Alignment,
			unsigned SectionID, llvm::StringRef SectionName) override {
		return allocateSection(Purpose::Code, Size, Alignment);
	}
	uint8_t* allocateDataSection (uintptr_t Size, unsigned Alignment,
			unsigned SectionID, llvm::StringRef SectionName, bool isReadOnly) override {
		return allocateSection(isReadOnly ? Purpose::ROData : Purpose::RWData, Size, Alignment);
	}

	uint8_t* allocateSection (Purpose Purpose, uintptr_t Size, unsigned Alignment) {
		if (!Alignment)
			Alignment = 16;
		assert(!(Alignment & (Alignment - 1)) && "Alignment must be a power of two.");

		auto& allocator = allocators[Purpose];
			
		auto& sec = allocator.sections.emplace_back();

		using namespace llvm::sys;
			
		sec.alloc(Size);

		if ((uintptr_t)sec.ptr != (((uintptr_t)sec.ptr + Alignment - 1) & ~(uintptr_t)(Alignment - 1))) {
			// FIXME: Alignment failure, should only happen if Alignment > OS page size, which could be disallowed in practice
			return nullptr;
		}

		// Return aligned address
		return (uint8_t*)sec.ptr;
	}

	std::error_code applyMemoryGroupPermissions (Allocator& allocator, llvm::sys::Memory::ProtectionFlags Permissions) {
		std::error_code ec;
		for (auto& sec : allocator.sections) {
			sec.protect(Permissions);
		}
		return ec;
	}

	bool finalizeMemory (std::string* ErrMsg = nullptr) override {
		// TODO: Assume that finalizeMemory is only called once and that no allocations happen after it
		// if I ever want to load and unload modules dynamically I'm going to change this
		std::error_code ec;
		using namespace llvm::sys;

		// Make code memory executable.
		ec = applyMemoryGroupPermissions(allocators[Code], (Memory::ProtectionFlags)(Memory::MF_READ | Memory::MF_EXEC));
		if (ec) {
			if (ErrMsg) *ErrMsg = ec.message();
			return true;
		}

		// Make read-only data memory read-only.
		ec = applyMemoryGroupPermissions(allocators[ROData], Memory::MF_READ);
		if (ec) {
			if (ErrMsg) *ErrMsg = ec.message();
			return true;
		}

		// Read-write data memory already has the correct permissions

		// Some platforms with separate data cache and instruction cache require
		// explicit cache flush, otherwise JIT code manipulations (like resolved
		// relocations) will get to the data cache but not to the instruction cache.
		invalidateInstructionCache();

		return false;
	}

	void invalidateInstructionCache() {
		for (auto& sec : allocators[Code].sections)
			llvm::sys::Memory::InvalidateInstructionCache(sec.ptr, sec.alloc_size);
	}

};
