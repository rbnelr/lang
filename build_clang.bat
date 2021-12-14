@cls

@set include=-I../common/tracy
@set srcs=util/console_colors.cpp util/file_io.cpp util/string.cpp util/timer.cpp main.cpp
@set warn=-Wno-unused-value -Wno-switch

clang --target=x86_64-w64-mingw32 -std=c++17 %warn% %include% %srcs% -o lang_clang.exe
