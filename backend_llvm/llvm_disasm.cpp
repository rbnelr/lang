#include "llvm_pch.hpp"
#include "llvm_disasm.hpp"
#include "frontend/builtins.hpp"

#include <regex>

struct DisasmPrinter {
	LLVMDisasmContextRef DCR;
	LoadedSections& sections;
	
	size_t code_bytes_len = 10; // 0 to not print code bytes

	bool print_strlit = true;
	int strlit_maxlen = 20;
	
	DisasmPrinter (llvm::Triple const& TT, LoadedSections& sections): sections{sections} {
		DCR = LLVMCreateDisasm(TT.str().c_str(), NULL, 0, NULL, NULL);

		LLVMSetDisasmOptions(DCR, 0
			| LLVMDisassembler_Option_UseMarkup
			| LLVMDisassembler_Option_AsmPrinterVariant

			//| LLVMDisassembler_Option_SetInstrComments // only comment I saw was no very useful:  movsd  xmm6, qword ptr [rip + 4074]    # xmm6 = mem[0],zero
			//| LLVMDisassembler_Option_PrintImmHex // doesn't work?
		);

		for (auto* bi : BUILTIN_FUNCS)
			sections.symbols.push_back({ (void*)bi->builtin_func_ptr, std::string(bi->ident) });
		
		sections.sort_symbols();
	}
	~DisasmPrinter () {
		LLVMDisasmDispose(DCR);
	}

	void print_disasm () {
		
		print_seperator("Disassembly:");

	#if 0
		print_seperator("Symbols:", '-');
		for (auto& sym : sections.symbols) {
			printf("%24s: %p\n", sym.name.c_str(), sym.addr);
		}
	#endif

		for (auto& seg : sections.segments) {
			
			printf("\n");
			print_seperator(prints("%s (size: %llu)", seg.get_name(), seg.size), '-');
			
			if (seg.size == 0) {
				printf("<empty>\n");
				return;
			}
			else {
				if (seg.type & MMAP_EXEC) {
					print_code_section((uint8_t*)seg.addr, seg.size);
				}
				else {
					print_data_section((uint8_t*)seg.addr, seg.size);
				}
			}

		}
	}

	// Print data sections like:
	// <loaded addr>    <offs>  <hex bytes>                          <char bytes>
	// 000001A6F5431000 0000 |  46696262 6f6e6163 693a207b 697d207b  Fibbonaci: {i} {
	// 000001A6F5431010 0010 |  697d0020 7b697d00 0a007371 7274287b  i}. {i}...sqrt({
	void print_data_section (uint8_t* data, size_t size) {
		int width = 16;

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

	// Print code sections like:
	// <loaded addr>    <offs>  <hex bytes>                    <disasm code>                       <comment with addr info>
	//                        @main:
	// 000001A6F5430000 0000 |  48 83 EC 28                    sub    rsp, 40                      #
	// 000001A6F5430004 0004 |  E8 27 00 00 00                 call   39                           # @fib_iter
	void print_code_section (uint8_t* data, size_t size) {
		
		// TODO: This is totally x64 specific! But ideally the disasm library would just provide enough info so it wouldn't be x64-specific

		struct InstructionStr {
			uint8_t*     ptr;
			size_t       length;
			std::string  str;
			std::string  comment;
		};
		std::vector<InstructionStr> disasm;
		
		int next_label = 0;
		struct Label {
			uint8_t* addr;
			int      id;
		};
		std::vector<Label> labels;
		
		disasm.reserve(4096);
		labels.reserve(128);

		// extract destination address from "rip + <offs>" "call <offs>" or "jmp <offs>"
		auto print_comment = [&] (char* str, uint8_t* rip_base) -> std::string {

			auto after_substr = [] (char* str, strview* out_substr) -> const char* {
				strview strs[] = {
					"rip +",
					"call",
					"jmp", "je", "jz", "jne", "jge", "jg", "jle", "jl",
				};

				for (auto substr : strs) {
					for (int i=0; str[i] != '\0'; ++i) {
						if (starts_with(&str[i], substr)) {
							char const* cur = &str[i + substr.size()];
							if (*cur < 'a' || *cur > 'z') { // exclude "jle" when matching "jl"
								while (*cur == ' ' || *cur == '\t') cur++;
							
								*out_substr = substr;
								return cur;
							}
						}
					}
				}
				return nullptr;
			};

			strview substr;
			auto offs_str = after_substr(str, &substr);
			if (!offs_str) return {};

			uint64_t rip_offs = (uint64_t)atoll(offs_str);
			if (rip_offs == 0)
				return {};

			auto* ptr = rip_base + rip_offs;

			LoadedSections::Symbol sym = {};

			for (auto& Symbol : sections.symbols) {
				if (ptr < Symbol.addr)
					break;
				sym = Symbol;
			}
			if (sym.addr == nullptr)
				return {};

			//printf(" %p", ptr);
				
			int sym_offs = (int)(ptr - (uint8_t*)sym.addr);

			if (sym_offs == 0) {
				if (print_strlit && starts_with(sym.name, ".Lstrlit.")) {
					auto str = escape_string_capped((char*)ptr, strlit_maxlen);
					return prints("\"%s\"", str.c_str());
				}
				else {
					return prints("@%.*s", (int)sym.name.size(), sym.name.data());
				}
			}
			else {
				if (substr[0] == 'j') {
					// is a jump to a label
					auto id = next_label++;
					labels.push_back({ ptr, id });

					return prints(".L%d", id);
				}
				else {
					return prints("@%.*s %+d", (int)sym.name.size(), sym.name.data(), sym_offs);
				}
			}
		};
		
		char strbuf[64];

		uint8_t* instr = data;

		auto end = data + size;

		while (instr < end) {
			size_t instr_bytes = LLVMDisasmInstruction(DCR, instr, end - instr, (uint64_t)instr, strbuf, ARRLEN(strbuf));
			
			assert(instr_bytes > 0); // should always disassemble correctly as long as we pass valid bytes into LLVMDisasmInstruction
			assert(strnlen(strbuf, ARRLEN(strbuf)) < ARRLEN(strbuf)-1); // make sure string is actually null terminated
			
			if (instr_bytes == 0)
				break;

			auto str = tabs2spaces_for_terminal(strbuf);
			auto comment = print_comment(strbuf, instr + instr_bytes);

			disasm.push_back({ instr, instr_bytes, std::move(str), std::move(comment) });

			instr += instr_bytes;
		}
		
		std::stable_sort(labels.begin(), labels.end(), [] (Label const& l, Label const& r) {
			return std::less<uintptr_t>()((uintptr_t)l.addr, (uintptr_t)r.addr);
		});

		auto cur_sym = sections.symbols.begin();
		auto cur_lbl = labels.begin();

		for (auto& instr : disasm) {
			uint64_t offs = instr.ptr - data;

			while (cur_sym != sections.symbols.end() && cur_sym->addr < instr.ptr) cur_sym++;
			while (cur_sym != sections.symbols.end() && cur_sym->addr == instr.ptr) {
				if (ansi_color_supported) fputs(ANSI_COLOR_GREEN, stdout);
				printf("\n---------------------- @%.*s:\n", (int)cur_sym->name.size(), cur_sym->name.data());
				cur_sym++;
			}

			while (cur_lbl != labels.end() && cur_lbl->addr < instr.ptr) cur_lbl++;
			while (cur_lbl != labels.end() && cur_lbl->addr == instr.ptr) {
				if (ansi_color_supported) fputs(ANSI_COLOR_BOLD_BLUE, stdout);
				printf("                       .L%d:\n", cur_lbl->id);
				cur_lbl++;
			}
			
			if (ansi_color_supported) fputs(ANSI_COLOR_BOLD_BLACK, stdout);
			printf("%p %04" PRIX64 " |  ", instr.ptr, offs);

			if (options.disasm_code_bytes) {
				for (size_t i=0; i<instr.length; ++i)
					printf("%02X ", instr.ptr[i]);
				for (size_t i=instr.length; i<code_bytes_len; ++i)
					printf("   ");
			}
			
			if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stdout);
			printf("%-36s", instr.str.c_str());

			if (!instr.comment.empty()) {
				if (ansi_color_supported) fputs(ANSI_COLOR_BOLD_BLACK, stdout);
				printf(" # %s", instr.comment.c_str());
			}
			printf("\n");
		}
		
		if (ansi_color_supported) fputs(ANSI_COLOR_RESET, stdout);
	}
	
	// turn  "\tmov\trax, 8"  into  "mov     rax, 8"
	// because printing this on the console with printf width specifiers does not work as expected
	// and I didn't wanna find out if I can get it to work somehow
	std::string tabs2spaces_for_terminal (const char* str) {
		std::string out;
		out.reserve(32);
			
		size_t i = 0;

		while (str[i] == '\t') i++; // skip inital tabs

		while (str[i] != '\0') {
			if (str[i] == '\t') {
				size_t spaces = 8 - (i % 8);
				for (size_t j=0; j<spaces; ++j) {
					out.push_back(' '); // align to next multiple of tab_width chars
				}
			}
			else {
				out.push_back(str[i]);
			}
			i++;
		}

		return out;
	}
};

void print_llvm_disasm (llvm::Triple const& TT, LoadedSections& sections) {
	DisasmPrinter printer { TT, sections };
	printer.print_disasm();
}