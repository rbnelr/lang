#include "llvm_pch.hpp"
#include "llvm_disasm.hpp"
#include "llvm_sec_mem_manager.hpp"
#include "frontend/builtins.hpp"

#include <regex>


struct DisasmPrinter {
	LLVMDisasmContextRef DCR;

	DisasmPrinter (llvm::Triple const& TT) {
		DCR = LLVMCreateDisasm(TT.str().c_str(), NULL, 0, NULL, NULL);

		LLVMSetDisasmOptions(DCR,
			LLVMDisassembler_Option_UseMarkup |
			LLVMDisassembler_Option_AsmPrinterVariant
		);
	}
	~DisasmPrinter () {
		LLVMDisasmDispose(DCR);
	}

	struct Symbol {
		llvm::StringRef name;
		const uint8_t*  addr;
	};
	std::vector<Symbol> symbols;

	void handle_symbols (llvm::RuntimeDyld& dyld, llvm::object::ObjectFile& obj) {
		print_seperator("Symbols", '-');

		for (auto& sym : obj.symbols()) {
			auto getname = sym.getName();
			if (!getname) continue; // skip on error

			auto name = getname.get();
			auto eval_sym = dyld.getSymbol(name);
			auto addr = (const uint8_t*)eval_sym.getAddress();

			if (options.disasm_print_symbols)
				printf("%-16.*s : %p\n", (int)name.size(), name.data(), addr);

			if (addr != nullptr) {
				symbols.push_back({ name, addr });
			}
		}

		for (auto* bi : builtin_funcs)
			symbols.push_back({ { bi->ident.data(), bi->ident.size() }, (const uint8_t*)bi->builtin_func_ptr });

		std::sort(symbols.begin(), symbols.end(), [] (Symbol const& l, Symbol const& r) {
			return std::less<uintptr_t>()((uintptr_t)l.addr, (uintptr_t)r.addr);
		});
	}

	void print_disasm (
			llvm::RuntimeDyld& dyld,
			llvm::object::ObjectFile& obj,
			llvm::RuntimeDyld::LoadedObjectInfo& loadedObj,
			SectionMemoryManager& sec_mem) {

		print_seperator("Disassembly:");

		handle_symbols(dyld, obj);
		
		for (auto& alloc : sec_mem.allocators)
		for (auto& sec : alloc.sections) {

			size_t reported_size;

			// Search section list for the one with the name (with a proper LLVM API our SectionMemoryManager would know about this already)
			// TODO: copy and modify LLVM backend?
			auto find_section = [&] () {
				// HACK: Any way to query which Sections exist in the RuntimeDyld is not made public for some reason
				// This is despite the fact that I can ask for Section addresses and (bogus) sizes by section ID
				// so exposing things by section ID yet not exposing the SectionRef -> sectionID mapping is just nonsensical imho
				// Since I can't find a way to easily access info about the section and I think I should be able to (deriving etc. all are not possible)
				// I'm just going to hack it with a pointer cast, sue me.
				using Map = llvm::RuntimeDyld::LoadedObjectInfo::ObjSectionToIDMap;
				auto& ObjSectionToID = *(Map*)((char*)&loadedObj + sizeof(loadedObj) - sizeof(Map));

				for (auto& sec2ID : ObjSectionToID) {
					//auto sec_getname = sec2ID.first.getName();
					auto sec_id = sec2ID.second;

					auto loadAddr = dyld.getSectionLoadAddress(sec_id);
					reported_size = dyld.getSectionContent(sec_id).size();

					if (sec.ptr == (void*)loadAddr) {
						return sec2ID.first;
					}
				}

				assert(false);
				return llvm::object::SectionRef{};
			};

			llvm::object::SectionRef secref = find_section();

			auto getname = secref.getName();
			if (!getname) continue; // skip on error

			auto name = getname.get();

			print_seperator(strview{ name.data(), name.size() }, '-');
			printf(";; OS alloc size       : %8llu\n"
			       ";; LLVM alloc size     : %8llu\n"
			       ";; LLVM reported size  : %8llu\n", sec.alloc_size, sec.sec_size, reported_size);
			
			auto data = (const uint8_t*)sec.ptr;
			auto size = sec.sec_size;
			
			if (size == 0) {
				printf("<empty>\n");
				return;
			}
			else {
				if (secref.isText()) {
					print_code_section(data, size);
				}
				else {
					print_data_section(data, size);
				}
			}
		}
	}

	void print_data_section (const uint8_t* data, size_t size) {
		int width = 16;

		size_t offs = 0;
		for (size_t offs = 0; offs < size; offs += width) {
			printf("%p %04" PRIx64 " | ", data + offs, offs);

			for (size_t i=0; i<16; ++i) {
				if (i % 4 == 0) putchar(' ');

				if (offs+i < size)
					printf("%02x", data[offs+i]);
				else
					printf("  "); // print spaces to fill non-aligned end
			}

			printf("  ");

			for (size_t i=0; i<16 && offs+i < size; ++i) {
				char c = (char)data[offs+i];
				if (isascii(c) && iscntrl(c)) c = '.';
				putchar(c);
			}
			
			printf("\n");
		}
	}

	void print_code_section (const uint8_t* data, size_t size) {
		size_t offs = 0;
		size_t remain = size;
		
		size_t cbytes_len = 10; // 0 to not print code bytes

		char str[64];
		llvm::SmallString<64> str_untab;
		// turn  "\tmov\trax, 8"  into  "mov     rax, 8"
		// because printing this on the console with printf width specifiers does not work as expected
		// and I didn't wanna find out if I can get it to work somehow

		auto untabbify = [&] () {
			size_t tab_width = 8;

			str_untab.clear();
			
			size_t i = 0;

			while (str[i] == '\t') i++; // skip inital tabs

			while (str[i] != '\0') {
				if (str[i] == '\t') {
					size_t spaces = 8 - (i % 8);
					for (size_t j=0; j<spaces; ++j) {
						str_untab.push_back(' '); // align to next multiple of tab_width chars
					}
				}
				else {
					str_untab.push_back(str[i]);
				}
				i++;
			}

			str_untab.push_back('\0');
		};

		auto print_comment = [&] () {
			char* int_str = nullptr;

			for (int i=0; i<ARRLEN(str) && str[i] != '\0'; ++i) {
				if (str[i] == ' ' && str[i+1] >= '0' && str[i+1] <= '9') {
					int_str = &str[i+1];
					break;
				}
			}

			if (int_str == nullptr)
				return;

			uint64_t val = (uint64_t)atoll(int_str);
			
			printf(" 0x%llx", val);

			auto* ptr = (const uint8_t*)val;
			for (auto& sym : symbols) {
				if (sym.addr == ptr) {
					printf(" @%.*s", (int)sym.name.size(), sym.name.data());
					break;
				}
			}
		};

		auto cur_sym = symbols.begin();

		// prints string indented by a few spaces
		while (size_t sz = LLVMDisasmInstruction(DCR, (uint8_t*)&data[offs], remain, offs, str, ARRLEN(str))) {
			while (cur_sym != symbols.end() && cur_sym->addr < data + offs) {
				cur_sym++;
			}
			if (cur_sym != symbols.end() && cur_sym->addr == data + offs) {
				printf("\n                       @%.*s:\n",
					(int)cur_sym->name.size(), cur_sym->name.data());
			}
			
			printf("%p %04" PRIX64 " |  ", data + offs, offs);

			if (cbytes_len > 0) {
				for (size_t i=0; i<sz; ++i)
					printf("%02X ", data[offs+i]);
				for (size_t i=sz; i<cbytes_len; ++i)
					printf("   ");
			}

			untabbify();
			
			printf(" %-35s #", str_untab.data());

			print_comment();

			printf("\n");

			offs += sz;
			remain -= sz;
		}
	}
};

void print_llvm_disasm (
		llvm::Triple const& TT,
		llvm::RuntimeDyld& dyld,
		llvm::object::ObjectFile& obj,
		llvm::RuntimeDyld::LoadedObjectInfo& loadedObj,
		SectionMemoryManager& sec_mem) {

	DisasmPrinter printer { TT };
	printer.print_disasm(dyld, obj, loadedObj, sec_mem);
}
