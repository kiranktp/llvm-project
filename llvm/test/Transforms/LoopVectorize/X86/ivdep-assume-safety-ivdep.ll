; RUN: opt < %s -O2 -S 2>&1 | FileCheck %s
; The C++ code for this test is:
; void ignore_vec_dep(int *a, int *b, int k, int c, int m)
; {
;   #pragma clang loop ivdep(enable)
;   for (int i = 0; i < m; i++)
;     a[i] = a[i] + a[i - 1];
; }
; With ivdep, this test will fail, ie vectorization will not happen.
; With assume_safety, the test will pass (ivdep-assume-safety-assume-safety.ll)

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable mustprogress
define dso_local void @_Z14ignore_vec_depPiS_iii(i32* %a, i32* %b, i32 %k, i32 %c, i32 %m) #0 {
entry:
  %a.addr = alloca i32*, align 8
  %b.addr = alloca i32*, align 8
  %k.addr = alloca i32, align 4
  %c.addr = alloca i32, align 4
  %m.addr = alloca i32, align 4
  %i = alloca i32, align 4
  store i32* %a, i32** %a.addr, align 8
  store i32* %b, i32** %b.addr, align 8
  store i32 %k, i32* %k.addr, align 4
  store i32 %c, i32* %c.addr, align 4
  store i32 %m, i32* %m.addr, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, i32* %i, align 4
  %1 = load i32, i32* %m.addr, align 4
  %cmp = icmp slt i32 %0, %1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %2 = load i32*, i32** %a.addr, align 8
  %3 = load i32, i32* %i, align 4
  %idxprom = sext i32 %3 to i64
  %arrayidx = getelementptr inbounds i32, i32* %2, i64 %idxprom
  %4 = load i32, i32* %arrayidx, align 4
  %5 = load i32*, i32** %a.addr, align 8
  %6 = load i32, i32* %i, align 4
  %sub = sub nsw i32 %6, 1
  %idxprom1 = sext i32 %sub to i64
  %arrayidx2 = getelementptr inbounds i32, i32* %5, i64 %idxprom1
  %7 = load i32, i32* %arrayidx2, align 4
  %add = add nsw i32 %4, %7
  %8 = load i32*, i32** %a.addr, align 8
  %9 = load i32, i32* %i, align 4
  %idxprom3 = sext i32 %9 to i64
  %arrayidx4 = getelementptr inbounds i32, i32* %8, i64 %idxprom3
  store i32 %add, i32* %arrayidx4, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %10 = load i32, i32* %i, align 4
  %inc = add nsw i32 %10, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond, !llvm.loop !2

for.end:                                          ; preds = %for.cond
  ret void
}

attributes #0 = { noinline nounwind uwtable mustprogress "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 13.0.0"}
!2 = distinct !{!2, !3, !4, !5}
!3 = !{!"llvm.loop.mustprogress"}
!4 = !{!"llvm.loop.vectorize.ivdep.enable", i1 true}
!5 = !{!"llvm.loop.vectorize.enable", i1 true}

; CHECK: {{.*}}loop not vectorized: the optimizer was unable to perform the requested transformation{{.*}}