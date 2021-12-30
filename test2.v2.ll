; ModuleID = 'test2.v1.ll'
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
bb:
  %i = mul nsw i32 5, 10
  %i3 = sub nsw i32 %i, 5
  %i4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @"??_C@_0BB@GDLPCHKP@Fibbonaci?3?5?$CFd?5?$CFd?$AA@", i64 0, i64 0), i32 0, i32 1)
  br label %bb5

bb5:                                              ; preds = %bb12, %bb
  %.02 = phi i32 [ 0, %bb ], [ %.01, %bb12 ]
  %.01 = phi i32 [ 1, %bb ], [ %i10, %bb12 ]
  %.0 = phi i32 [ 0, %bb ], [ %i13, %bb12 ]
  %i6 = sub nsw i32 %i3, 2
  %i7 = icmp slt i32 %.0, %i6
  br i1 %i7, label %bb9, label %bb8

bb8:                                              ; preds = %bb5
  br label %bb14

bb9:                                              ; preds = %bb5
  %i10 = add nsw i32 %.02, %.01
  %i11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"??_C@_03EBJOKFHF@?5?$CFd?$AA@", i64 0, i64 0), i32 %i10)
  br label %bb12

bb12:                                             ; preds = %bb9
  %i13 = add nsw i32 %.0, 1
  br label %bb5, !llvm.loop !4

bb14:                                             ; preds = %bb8
  %i15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"??_C@_01EEMJAFIK@?6?$AA@", i64 0, i64 0))
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

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 1}
!3 = !{!"clang version 13.0.0"}
!4 = distinct !{!4, !5}
!5 = !{!"llvm.loop.mustprogress"}
