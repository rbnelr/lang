; ModuleID = 'test2.c'
source_filename = "test2.c"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc19.30.30706"

$"??_C@_0BB@GDLPCHKP@Fibbonaci?3?5?$CFd?5?$CFd?$AA@" = comdat any

$"??_C@_03EBJOKFHF@?5?$CFd?$AA@" = comdat any

$"??_C@_01EEMJAFIK@?6?$AA@" = comdat any

@"??_C@_0BB@GDLPCHKP@Fibbonaci?3?5?$CFd?5?$CFd?$AA@" = linkonce_odr dso_local unnamed_addr constant [17 x i8] c"Fibbonaci: %d %d\00", comdat, align 1
@"??_C@_03EBJOKFHF@?5?$CFd?$AA@" = linkonce_odr dso_local unnamed_addr constant [4 x i8] c" %d\00", comdat, align 1
@"??_C@_01EEMJAFIK@?6?$AA@" = linkonce_odr dso_local unnamed_addr constant [2 x i8] c"\0A\00", comdat, align 1

; Function Attrs: nounwind uwtable
define dso_local void @test() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = bitcast i32* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %8) #3
  %9 = bitcast i32* %2 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %9) #3
  store i32 5, i32* %2, align 4, !tbaa !4
  %10 = bitcast i32* %3 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %10) #3
  store i32 10, i32* %3, align 4, !tbaa !4
  %11 = load i32, i32* %2, align 4, !tbaa !4
  %12 = load i32, i32* %3, align 4, !tbaa !4
  %13 = mul nsw i32 %11, %12
  %14 = sub nsw i32 %13, 5
  store i32 %14, i32* %1, align 4, !tbaa !4
  %15 = bitcast i32* %3 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %15) #3
  %16 = bitcast i32* %2 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %16) #3
  %17 = bitcast i32* %4 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %17) #3
  store i32 0, i32* %4, align 4, !tbaa !4
  %18 = bitcast i32* %5 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %18) #3
  store i32 1, i32* %5, align 4, !tbaa !4
  %19 = load i32, i32* %5, align 4, !tbaa !4
  %20 = load i32, i32* %4, align 4, !tbaa !4
  %21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @"??_C@_0BB@GDLPCHKP@Fibbonaci?3?5?$CFd?5?$CFd?$AA@", i64 0, i64 0), i32 %20, i32 %19)
  %22 = bitcast i32* %6 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %22) #3
  store i32 0, i32* %6, align 4, !tbaa !4
  br label %23

23:                                               ; preds = %40, %0
  %24 = load i32, i32* %6, align 4, !tbaa !4
  %25 = load i32, i32* %1, align 4, !tbaa !4
  %26 = sub nsw i32 %25, 2
  %27 = icmp slt i32 %24, %26
  br i1 %27, label %30, label %28

28:                                               ; preds = %23
  %29 = bitcast i32* %6 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %29) #3
  br label %43

30:                                               ; preds = %23
  %31 = bitcast i32* %7 to i8*
  call void @llvm.lifetime.start.p0i8(i64 4, i8* %31) #3
  %32 = load i32, i32* %4, align 4, !tbaa !4
  %33 = load i32, i32* %5, align 4, !tbaa !4
  %34 = add nsw i32 %32, %33
  store i32 %34, i32* %7, align 4, !tbaa !4
  %35 = load i32, i32* %7, align 4, !tbaa !4
  %36 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"??_C@_03EBJOKFHF@?5?$CFd?$AA@", i64 0, i64 0), i32 %35)
  %37 = load i32, i32* %5, align 4, !tbaa !4
  store i32 %37, i32* %4, align 4, !tbaa !4
  %38 = load i32, i32* %7, align 4, !tbaa !4
  store i32 %38, i32* %5, align 4, !tbaa !4
  %39 = bitcast i32* %7 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %39) #3
  br label %40

40:                                               ; preds = %30
  %41 = load i32, i32* %6, align 4, !tbaa !4
  %42 = add nsw i32 %41, 1
  store i32 %42, i32* %6, align 4, !tbaa !4
  br label %23, !llvm.loop !8

43:                                               ; preds = %28
  %44 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"??_C@_01EEMJAFIK@?6?$AA@", i64 0, i64 0))
  %45 = bitcast i32* %5 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %45) #3
  %46 = bitcast i32* %4 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %46) #3
  %47 = bitcast i32* %1 to i8*
  call void @llvm.lifetime.end.p0i8(i64 4, i8* %47) #3
  ret void
}

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

declare dso_local i32 @printf(i8*, ...) #2

attributes #0 = { nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { argmemonly nofree nosync nounwind willreturn }
attributes #2 = { "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 1}
!3 = !{!"clang version 13.0.0"}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = distinct !{!8, !9}
!9 = !{!"llvm.loop.mustprogress"}
