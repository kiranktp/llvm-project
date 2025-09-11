// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -std=c++11 -emit-llvm -o - %s | FileCheck %s

void test0(int *a, int *b, int LEN_1D) {
  // CHECK-LABEL: @{{.*}}test0{{.*}}(
  // CHECK: br label {{.*}}, !llvm.loop ![[LOOP0:.*]]

#pragma clang loop vectorize(enable)
  for (int i = 0; i < LEN_1D; i++)
    a[b[i]] = a[b[i]] + 1;
}

void test1(int *a, int *b, int LEN_1D) {
  // CHECK-LABEL: @{{.*}}test1{{.*}}(
  // CHECK: br label {{.*}}, !llvm.loop ![[LOOP1:.*]]

#pragma clang loop ivdep(enable)
  for (int i = 0; i < LEN_1D; i++)
    a[b[i]] = a[b[i]] + 1;
}

void test2(int *a, int *b, int LEN_1D) {
  // CHECK-LABEL: @{{.*}}test2{{.*}}(
  // CHECK: br label {{.*}}, !llvm.loop ![[LOOP2:.*]]

#pragma clang loop vectorize(enable) ivdep(enable)
  for (int i = 0; i < LEN_1D; i++)
    a[b[i]] = a[b[i]] + 1;
}

// CHECK:      ![[LOOP0]] = distinct !{![[LOOP0]], !3, !4}
// CHECK-NEXT: !3 = !{!"llvm.loop.mustprogress"}
// CHECK-NEXT: !4 = !{!"llvm.loop.vectorize.enable", i1 true}
// CHECK-NEXT: ![[LOOP1]] = distinct !{![[LOOP1]], !3, !6, !4}
// CHECK-NEXT: !6 = !{!"llvm.loop.vectorize.ivdep.enable", i1 true}
// CHECK-NEXT: ![[LOOP2]] = distinct !{![[LOOP2]], !3, !6, !4}