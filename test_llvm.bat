
@set path=..\libLLVM_Release\bin;%path%

clang -O3 -Xclang -disable-llvm-passes -S -emit-llvm test2.c -o test2.v1.ll
opt -S -mem2reg -instnamer test2.v1.ll -o test2.v2.ll
