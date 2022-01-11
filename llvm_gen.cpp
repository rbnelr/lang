
#include <inttypes.h>

#pragma warning(push, 0)
#pragma warning (disable : 4244)

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h" // warning C4244: 'initializing': conversion from '_Ty' to '_Ty2', possible loss of data

#include "xcoff.h"

#pragma warning(pop)

#include "windows.h"

#undef IMAGE_FILE_MACHINE_AMD64

using namespace llvm;
using namespace llvm::orc;

ExitOnError ExitOnErr;

#define TEST 1

struct CoffLoader {
	// from https://wiki.osdev.org/COFF
	// matches what's found in llvm-project\llvm\include\llvm\Object\COFF.h
	// does _not_ match        llvm-project\llvm\include\llvm\BinaryFormat\COFF.h
	struct Header {
		uint16_t  magic;          /* Magic number */	
		uint16_t  num_sections;   /* Number of Sections */
		uint32_t  timedat;        /* Time & date stamp */
		uint32_t  symbol_table;   /* File pointer to Symbol Table */
		uint32_t  num_symbols;    /* Number of Symbols */
		uint16_t  opt_header_sz;  /* sizeof(Optional Header) */
		uint16_t  flags;          /* Flags */
	};

	struct Section {
		char      name[8];        /* Section Name */
		uint32_t  phys_addr;      /* Physical Address */
		uint32_t  virt_addr;      /* Virtual Address */
		uint32_t  size;           /* Section Size in Bytes */
		uint32_t  section_ptr;    /* File offset to the Section data */
		uint32_t  reloc_table;    /* File offset to the Relocation table for this Section */
		uint32_t  linenum_table;  /* File offset to the Line Number table for this Section */
		uint16_t  num_relocs;     /* Number of Relocation table entries */
		uint16_t  num_linenums;   /* Number of Line Number table entries */
		uint32_t  flags;          /* Flags for this section */
	};
	struct Symbol {
		char      name[8];        /* Symbol Name */
		uint32_t  value;          /* Value of Symbol */
		int16_t   section;        /* Section Number */
		uint16_t  type;           /* Symbol Type */
		uint8_t   sclass;         /* Storage Class */
		uint8_t   numaux;         /* Auxiliary Count */
	};
	struct Relocation {
		int32_t   virt_addr;      /* Reference Address */
		int32_t   symbol;         /* Symbol index */
		uint16_t  type;           /* Type of relocation */
	};

	Header*  header;
	Section* sections;
	Symbol*  symbols;

	int text_scn = 0;

	void* executable = nullptr;
	size_t executable_sz = 0;

	~CoffLoader () {
		VirtualFree(executable, executable_sz, MEM_RELEASE | MEM_DECOMMIT);
	}

	void load_coff (char* file, size_t filesz) {
		executable_sz = filesz;

		header = (Header*)file;
		//assert(header->magic == COFF::IMAGE_FILE_MACHINE_AMD64); // 0x8664
		assert(header->opt_header_sz == 0);

		sections = (Section*)(header + 1);

		symbols = (Symbol*)(file + header->symbol_table);

		for (int32_t i=0; i < header->num_sections; ++i) {
			auto& sec = sections[i];

			auto* relocs = (Relocation*)(file + sec.reloc_table);

			if (strcmp(sec.name, ".text") == 0) {
				text_scn = i+1; // section numbers are 1-based
			}
		}

		executable = VirtualAlloc(NULL, executable_sz, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
		assert(executable);

		memcpy(executable, file, executable_sz);

		DWORD oldProtect;
		auto res = VirtualProtect(executable, executable_sz, PAGE_EXECUTE_READ, &oldProtect);
		assert(res);
	}

	void* find_func (const char* funcname) {
		if (text_scn == 0)
			return nullptr;

		auto* text_ptr = (char*)executable + sections[text_scn-1].virt_addr;

		for (uint32_t i=0; i < header->num_symbols; ++i) {
			auto& sym = symbols[i];
			if (strcmp(sym.name, funcname) == 0) {
				constexpr char C_EXT = 2;

				assert(sym.sclass == C_EXT && sym.section == text_scn);
				return text_ptr + sym.value;
			}
		}

		return nullptr;
	}
};

struct JIT {

#if TEST
	std::unique_ptr<LLVMContext> ctx;
	std::unique_ptr<Module>      modl;
#else
	ThreadSafeContext TSC;
	ThreadSafeModule  TSM;
	
	std::unique_ptr<ExecutionSession>  ES;
	
	std::unique_ptr<DataLayout>               DL;
	std::unique_ptr<MangleAndInterner>        Mangle;
	
	std::unique_ptr<RTDyldObjectLinkingLayer> ObjectLayer;
	std::unique_ptr<IRCompileLayer>           CompileLayer;
	
	JITDylib*                                 MainJD;
#endif

	void create () {
		// TODO: I can't pass -debug to my own app and expect LLVM to set this flag can I?
		// so just set it manually to print stuff?
	#ifndef NDEBUG
		//llvm::DebugFlag = true;
	#endif

		InitializeNativeTarget();
		InitializeNativeTargetAsmPrinter();
		InitializeNativeTargetAsmParser();

	#if TEST
		ctx = std::make_unique<LLVMContext>();
		modl = std::make_unique<Module>("llvm_test", *ctx);
	#else
		TSC = ThreadSafeContext(std::make_unique<LLVMContext>());
		TSM = ThreadSafeModule(std::make_unique<Module>("llvm_test", *TSC.getContext()), TSC);
		
		auto EPC = ExitOnErr( SelfExecutorProcessControl::Create() );
		ES = std::make_unique<ExecutionSession>(std::move(EPC));
		
		JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());
		
		DL = std::make_unique<DataLayout>(ExitOnErr( JTMB.getDefaultDataLayoutForTarget() ));
		
		Mangle = std::make_unique<MangleAndInterner>(*ES, *DL);
		
		ObjectLayer = std::make_unique<RTDyldObjectLinkingLayer>(*this->ES, []() { return std::make_unique<SectionMemoryManager>(); });
		
		CompileLayer = std::make_unique<IRCompileLayer>(*this->ES, *ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB)));
		
		MainJD = &ES->createBareJITDylib("<main>");
		
		MainJD->addGenerator(cantFail(
			DynamicLibrarySearchGenerator::GetForCurrentProcess(DL->getGlobalPrefix())
		));
		
		TSM.getModuleUnlocked()->setDataLayout(*DL);
		
		if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
			ObjectLayer->setOverrideObjectFlagsWithResponsibilityFlags(true);
			ObjectLayer->setAutoClaimResponsibilityForObjectSymbols(true);
		}
	#endif
	}

	void destroy () {
		//if (auto Err = ES->endSession())
		//	ES->reportError(std::move(Err));
	}

	typedef float (*func_t)(float arg0, float arg1);

	CoffLoader binary_loader;

	static Function* compile_test (LLVMContext* ctx, Module* modl) {

		auto Builder = IRBuilder<>(*ctx);

		std::vector<Type*> args = {
			Type::getFloatTy(*ctx),
			Type::getFloatTy(*ctx),
		};

		auto* FT = FunctionType::get(Type::getFloatTy(*ctx), args, false);

		auto* F = Function::Create(FT, Function::ExternalLinkage, "test", *modl);

		auto arg = F->args().begin();
		auto& arg0 = *arg++;
		auto& arg1 = *arg++;

		arg0.setName("arg0");
		arg1.setName("arg1");

		auto* BB = BasicBlock::Create(*ctx, "entry", F);
		Builder.SetInsertPoint(BB);

		//
		auto* a = &arg0;
		auto* b = ConstantFP::get(*ctx, APFloat(2.0f));

		auto* c = Builder.CreateFAdd(a, b, "addtmp");

		auto* d = &arg1;
		auto* e = Builder.CreateFMul(c, d, "multmp");

		// Finish off the function.
		Builder.CreateRet(e);

		verifyFunction(*F);

		modl->print(errs(), nullptr);

		return F;
	}

	void run_test () {

	#if TEST
		auto EPC = ExitOnErr( SelfExecutorProcessControl::Create() );
		auto ES = std::make_unique<ExecutionSession>(std::move(EPC));

		JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());

		auto DL = std::make_unique<DataLayout>(ExitOnErr( JTMB.getDefaultDataLayoutForTarget() ));

		modl->setDataLayout(*DL);

		auto TM = cantFail( JTMB.createTargetMachine() );
		auto SC = SimpleCompiler(*TM);

		auto cres = ExitOnErr( SC(*modl) );

		char* filedata = (char*)cres->getBufferStart();
		auto filesz = cres->getBufferSize();

		binary_loader.load_coff(filedata, filesz);
		
		auto fptr = (func_t)binary_loader.find_func("test");

	#else
		auto RT = MainJD->getDefaultResourceTracker();
		CompileLayer->add(RT, std::move(TSM));
		
		auto func = ES->lookup({MainJD}, (*Mangle)("test"));
		auto faddr = func->getAddress();
		
		auto fptr = (func_t)faddr;
	#endif
		
		float a0 = 5.0f, a1 = 0.5f;
		float res = fptr(a0, a1);
		
		printf("test(%f, %f) = %f\n", a0, a1, res);
	}
};

void llvm_test () {
	JIT jit;
	jit.create();

#if TEST
	jit.compile_test(jit.ctx.get(), jit.modl.get());
#else
	jit.compile_test(jit.TSC.getContext(), jit.TSM.getModuleUnlocked());
#endif

	jit.run_test();
	jit.destroy();

	exit(0);
}

//int main () {
//	llvm_test();
//	return 0;
//}
