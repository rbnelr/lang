#include "llvm_pch.hpp"
#include "llvm_backend.hpp"
#include "llvm_disasm.hpp"

struct JIT {

//// Resolver (why do I need this? possibly once I combine multiple modules?)
	class Resolver : public llvm::JITSymbolResolver {
	public:

		// TODO: is there a way to simply declare my builtins as symbols so this entire Resolver class would become unnesassary?
		//       or is this generally needed to link multiple modules together?
		std::map<llvm::StringRef, llvm::JITTargetAddress> external_funcs;

		Resolver () {}
		virtual ~Resolver() = default;
		
		// this seems to be called for function symbols that are not in the code
		// which includes my builtin functions
		// TODO: is there a way to simply declare my builtins as external (to be linked) functions?

		/// Returns the fully resolved address and flags for each of the given
		///        symbols.
		///
		/// This method will return an error if any of the given symbols can not be
		/// resolved, or if the resolution process itself triggers an error.
		virtual void lookup(const LookupSet &Symbols,
							OnResolvedFunction OnResolved) {

			std::map<llvm::StringRef, llvm::JITEvaluatedSymbol> results;

			for (auto& Sym : Symbols) {
				auto it = external_funcs.find(Sym);
				if (it != external_funcs.end()) {
					results.emplace(Sym, llvm::JITEvaluatedSymbol{
						it->second,
						llvm::JITSymbolFlags::Absolute | // TODO: do I need this?
						llvm::JITSymbolFlags::Callable
					});
				}
				else {
					assert(false);
				}
			}

			OnResolved(results);
		}

		// No idea what this is supposed to do, I always seem to be called with an empty LookupSet

		/// Returns the subset of the given symbols that should be materialized by
		/// the caller. Only weak/common symbols should be looked up, as strong
		/// definitions are implicitly always part of the caller's responsibility.
		virtual llvm::Expected<LookupSet>
		getResponsibilitySet(const LookupSet &Symbols) {
			LookupSet Result;
			assert(Symbols.size() == 0);
			return Result;
		}

		/// Specify if this resolver can return valid symbols with zero value.
		//virtual bool allowsZeroSymbols() { return false; }

	};
	void register_builtins () {
		for (auto& builtin : builtin_funcs) {
			resolver.external_funcs.emplace(SR(builtin->ident), (llvm::JITTargetAddress)builtin->builti_func_ptr);
		}
	}

//// Memory manager that handles code and code-data memory pages (memory for sections bascially)

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
				size_t                 sec_size;
				llvm::sys::MemoryBlock mem;

				// TODO: consider alignment here, for now I'm going to assume sections are never aligned to more than the alignment returned by this
				std::error_code alloc (size_t size, llvm::sys::Memory::ProtectionFlags flags) {
					sec_size = size;

					using namespace llvm::sys;
					std::error_code ec;
					mem = Memory::allocateMappedMemory(size, nullptr, flags, ec);

					assert(mem.allocatedSize() > size);
				#ifndef NDEBUG
					// mark actually allocated section memory as UNINIT
					memset(mem.base(),               _DBG_MAGIC_UNINIT, size);
					// mark not actually requested but allocated section memory as NONALLOC
					// NOTE: This region should never be accessed, since it's only allocated by chance, yet getSectionContent return part of this!!
					memset((char*)mem.base() + size, _DBG_MAGIC_NONALLOC, mem.allocatedSize() - size);
				#endif

					return ec;
				}

				std::error_code protect (llvm::sys::Memory::ProtectionFlags flags) {
					return llvm::sys::Memory::protectMappedMemory(mem, flags);
				}

				~Section () {
					using namespace llvm::sys;
					std::error_code ec = Memory::releaseMappedMemory(mem);
					// ??? when exactly is freeing valid memory supposed to fail?
				}
			};
			
			llvm::SmallVector<Section, 8> allocations;
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

		/// Allocates a memory block of (at least) the given size suitable for
		/// executable code.
		///
		/// The value of \p Alignment must be a power of two.  If \p Alignment is zero
		/// a default alignment of 16 will be used.
		uint8_t* allocateCodeSection (uintptr_t Size, unsigned Alignment,
				unsigned SectionID, llvm::StringRef SectionName) override {
			return allocateSection(Purpose::Code, Size, Alignment);
		}

		/// Allocates a memory block of (at least) the given size suitable for
		/// executable code.
		///
		/// The value of \p Alignment must be a power of two.  If \p Alignment is zero
		/// a default alignment of 16 will be used.
		uint8_t* allocateDataSection (uintptr_t Size, unsigned Alignment,
				unsigned SectionID, llvm::StringRef SectionName, bool isReadOnly) override {
			
			auto purpose = isReadOnly ? Purpose::ROData : Purpose::RWData;
			return allocateSection(purpose, Size, Alignment);
		}

		uint8_t* allocateSection (Purpose Purpose, uintptr_t Size, unsigned Alignment) {
			if (!Alignment)
				Alignment = 16;
			assert(!(Alignment & (Alignment - 1)) && "Alignment must be a power of two.");

			auto& allocator = allocators[Purpose];
			
			auto& sec = allocator.allocations.emplace_back();

			using namespace llvm::sys;
			
			std::error_code ec = sec.alloc(Size, (Memory::ProtectionFlags)(Memory::MF_READ | Memory::MF_WRITE));
			if (ec) {
				// FIXME: Add error propagation to the interface.
				return nullptr;
			}

			void* ptr = sec.mem.base();

			if ((uintptr_t)ptr != (((uintptr_t)ptr + Alignment - 1) & ~(uintptr_t)(Alignment - 1))) {
				// FIXME: Alignment failure, should only happen if Alignment > OS page size, which could be disallowed in practice
				return nullptr;
			}

			// Return aligned address
			return (uint8_t*)ptr;
		}

		std::error_code applyMemoryGroupPermissions (Allocator& allocator, llvm::sys::Memory::ProtectionFlags Permissions) {
			std::error_code ec;
			for (auto& sec : allocator.allocations) {
				if ((ec = sec.protect(Permissions)))
					break;
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
			for (auto& sec : allocators[Code].allocations)
				llvm::sys::Memory::InvalidateInstructionCache(sec.mem.base(), sec.mem.allocatedSize());
		}

	};


	Resolver                             resolver;
	SectionMemoryManager                 MM;
	
	llvm::RuntimeDyld                    dyld;

	llvm::Triple                         TT;

	std::unique_ptr<llvm::TargetMachine> TM;

	JIT (): resolver{}, MM{}, dyld{MM, resolver} {
		ZoneScoped;

		// TODO: I can't pass -debug to my own app and expect LLVM to set this flag can I?
		// so just set it manually to print stuff?
	#ifndef NDEBUG
		//llvm::DebugFlag = true;
	#endif

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();
		llvm::InitializeNativeTargetDisassembler();

		register_builtins();

		auto triple_str = llvm::sys::getProcessTriple();
		TT = llvm::Triple(triple_str);

		llvm::orc::JITTargetMachineBuilder JTMB(TT);

		TM = llvm::cantFail( JTMB.createTargetMachine() );
	}
	
	void setup_PM (llvm::legacy::PassManager& PM, llvm::raw_svector_ostream& ObjStream) {
		
		// mem2reg pass for alloca'd local vars
		//PM.add(llvm::createPromoteMemoryToRegisterPass());

		llvm::MCContext* Ctx;
		if (TM->addPassesToEmitMC(PM, Ctx, ObjStream)) {
			ExitOnErr( llvm::make_error<llvm::StringError>(
				"Target does not support MC emission",
				llvm::inconvertibleErrorCode())
			);
		}
	}

	void compile_and_load (llvm::Module* modl) {
		
		auto DL = TM->createDataLayout();

		modl->setDataLayout(DL);
		
		llvm::SmallVector<char, 0> ObjBufferSV;
		llvm::raw_svector_ostream ObjStream(ObjBufferSV);

		{
			llvm::legacy::PassManager PM;
			
			/* // Emit assembly file instead
			llvm::SmallVector<char, 0> DwoBufferSV;
			llvm::raw_svector_ostream DwoStream(DwoBufferSV);
		
			if (TM->addPassesToEmitFile(PM, ObjStream, &DwoStream, llvm::CGFT_AssemblyFile)) {
				ExitOnErr( llvm::make_error<llvm::StringError>(
					"Target does not support MC emission",
					llvm::inconvertibleErrorCode())
				);
			}

			PM.run(*modl);

			save_text_file("test.asm", ObjBufferSV.data());
			*/

			setup_PM(PM, ObjStream);

			PM.run(*modl);
		}

		llvm::SmallVectorMemoryBuffer ObjBuffer {
			std::move(ObjBufferSV),
			modl->getModuleIdentifier() + "-jitted-objectbuffer"
		};
			
		auto Obj = ExitOnErr(
			llvm::object::ObjectFile::createObjectFile(ObjBuffer.getMemBufferRef())
		);
			
		auto loadedObj = dyld.loadObject(*Obj);

		dyld.finalizeWithMemoryManagerLocking(); // calls resolveRelocations
		
		DisasmPrinter disasm(TT);
		disasm.print_disasm(dyld, *Obj, *loadedObj);
	}
	
	void jit_and_execute (llvm::Module* modl) {
		ZoneScoped;

		compile_and_load(modl);

		print_seperator("Execute JITed LLVM code:");

		typedef void (*main_fp)();
		auto fptr = (main_fp)dyld.getSymbol("main").getAddress();
		fptr();
	}
};

void llvm_jit_and_exec (llvm::Module* modl) {
	ZoneScoped;

	JIT jit {};
	jit.jit_and_execute(modl);
}
