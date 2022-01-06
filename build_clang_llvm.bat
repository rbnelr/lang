@echo off
cls

"../libLLVM_Release/bin/llvm-config" --cxxflags --ldflags --system-libs --libs core orcjit native>llvm_cmds.txt

set llvm_I=-IC:\Users\Me\Desktop\schoolcoding\projects\libLLVM_Release/include
set llvm_D=-D_FILE_OFFSET_BITS=64 -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
set llvm_L=-LC:\Users\Me\Desktop\schoolcoding\projects\libLLVM_Release/lib -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMCFGuard -lLLVMGlobalISel -lLLVMX86Desc -lLLVMX86Info -lLLVMMCDisassembler -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMDebugInfoMSF -lLLVMDebugInfoDWARF -lLLVMCodeGen -lLLVMOrcJIT -lLLVMPasses -lLLVMObjCARCOpts -lLLVMCoroutines -lLLVMipo -lLLVMInstrumentation -lLLVMVectorize -lLLVMScalarOpts -lLLVMLinker -lLLVMIRReader -lLLVMAsmParser -lLLVMInstCombine -lLLVMFrontendOpenMP -lLLVMBitWriter -lLLVMAggressiveInstCombine -lLLVMTransformUtils -lLLVMJITLink -lLLVMExecutionEngine -lLLVMTarget -lLLVMAnalysis -lLLVMProfileData -lLLVMRuntimeDyld -lLLVMOrcTargetProcess -lLLVMOrcShared -lLLVMObject -lLLVMTextAPI -lLLVMMCParser -lLLVMBitReader -lLLVMMC -lLLVMDebugInfoCodeView -lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMSupport -lLLVMDemangle
set llvm_Lsys=-lpsapi -lshell32 -lole32 -luuid -ladvapi32


set include=-I../common/tracy %llvm_I%
set srcs=llvm_gen.cpp
set warn=
set opt=-O3 -g
set targ=-target x86_64-pc-windows-gnu -pthread



@echo on
clang++ %targ% -std=c++17 %warn% %opt% %include% %llvm_D% %srcs% -o lang_clang.exe %llvm_L% %llvm_Lsys%
