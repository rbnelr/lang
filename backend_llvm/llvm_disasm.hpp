#pragma once
#include "llvm_pch.hpp"
#include "llvm_sec_mem_manager.hpp"

void print_llvm_disasm (
		llvm::Triple const& TT,
		llvm::RuntimeDyld& dyld,
		llvm::object::ObjectFile& obj,
		llvm::RuntimeDyld::LoadedObjectInfo& loadedObj,
		SectionMemoryManager& sec_mem);
