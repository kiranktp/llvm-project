// RUN: mlir-opt %s | mlir-opt | FileCheck %s


// CHECK-LABEL: func @baz
// something to call
llvm.func @baz()

// CHECK-LABEL: func @ops
// CHECK-SAME: (%[[I32:.*]]: i32, %[[FLOAT:.*]]: f32, %[[PTR1:.*]]: !llvm.ptr, %[[PTR2:.*]]: !llvm.ptr, %[[BOOL:.*]]: i1, %[[VPTR1:.*]]: vector<2x!llvm.ptr>)
func.func @ops(%arg0: i32, %arg1: f32,
          %arg2: !llvm.ptr, %arg3: !llvm.ptr,
          %arg4: i1, %arg5 : vector<2x!llvm.ptr>) {
// Integer arithmetic binary operations.
//
// CHECK: {{.*}} = llvm.add %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.sub %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.mul %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.udiv %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.sdiv %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.urem %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.srem %[[I32]], %[[I32]] : i32
// CHECK: %[[SCALAR_PRED0:.+]] = llvm.icmp "ne" %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.add %[[SCALAR_PRED0]], %[[SCALAR_PRED0]] : i1
// CHECK: %[[SCALAR_PRED1:.+]] = llvm.icmp "ne" %[[PTR1]], %[[PTR1]] : !llvm.ptr
// CHECK: {{.*}} = llvm.add %[[SCALAR_PRED1]], %[[SCALAR_PRED1]] : i1
// CHECK: %[[VEC_PRED:.+]] = llvm.icmp "ne" %[[VPTR1]], %[[VPTR1]] : vector<2x!llvm.ptr>
// CHECK: {{.*}} = llvm.add %[[VEC_PRED]], %[[VEC_PRED]] : vector<2xi1>
  %0 = llvm.add %arg0, %arg0 : i32
  %1 = llvm.sub %arg0, %arg0 : i32
  %2 = llvm.mul %arg0, %arg0 : i32
  %3 = llvm.udiv %arg0, %arg0 : i32
  %4 = llvm.sdiv %arg0, %arg0 : i32
  %5 = llvm.urem %arg0, %arg0 : i32
  %6 = llvm.srem %arg0, %arg0 : i32
  %7 = llvm.icmp "ne" %arg0, %arg0 : i32
  %typecheck_7 = llvm.add %7, %7 : i1
  %ptrcmp = llvm.icmp "ne" %arg2, %arg2 : !llvm.ptr
  %typecheck_ptrcmp = llvm.add %ptrcmp, %ptrcmp : i1
  %vptrcmp = llvm.icmp "ne" %arg5, %arg5 : vector<2x!llvm.ptr>
  %typecheck_vptrcmp = llvm.add %vptrcmp, %vptrcmp : vector<2 x i1>

// Integer overflow flags
// CHECK: {{.*}} = llvm.add %[[I32]], %[[I32]] overflow<nsw> : i32
// CHECK: {{.*}} = llvm.sub %[[I32]], %[[I32]] overflow<nuw> : i32
// CHECK: {{.*}} = llvm.mul %[[I32]], %[[I32]] overflow<nsw, nuw> : i32
// CHECK: {{.*}} = llvm.shl %[[I32]], %[[I32]] overflow<nsw, nuw> : i32
  %add_flag = llvm.add %arg0, %arg0 overflow<nsw> : i32
  %sub_flag = llvm.sub %arg0, %arg0 overflow<nuw> : i32
  %mul_flag = llvm.mul %arg0, %arg0 overflow<nsw, nuw> : i32
  %shl_flag = llvm.shl %arg0, %arg0 overflow<nuw, nsw> : i32

// Integer exact flag.
// CHECK: {{.*}} = llvm.sdiv exact %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.udiv exact %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.ashr exact %[[I32]], %[[I32]] : i32
// CHECK: {{.*}} = llvm.lshr exact %[[I32]], %[[I32]] : i32
  %sdiv_flag = llvm.sdiv exact %arg0, %arg0 : i32
  %udiv_flag = llvm.udiv exact %arg0, %arg0 : i32
  %ashr_flag = llvm.ashr exact %arg0, %arg0 : i32
  %lshr_flag = llvm.lshr exact %arg0, %arg0 : i32

// Integer disjoint flag.
// CHECK: {{.*}} = llvm.or disjoint %[[I32]], %[[I32]] : i32
  %or_flag = llvm.or disjoint %arg0, %arg0 : i32

// Floating point binary operations.
//
// CHECK: {{.*}} = llvm.fadd %[[FLOAT]], %[[FLOAT]] : f32
// CHECK: {{.*}} = llvm.fsub %[[FLOAT]], %[[FLOAT]] : f32
// CHECK: {{.*}} = llvm.fmul %[[FLOAT]], %[[FLOAT]] : f32
// CHECK: {{.*}} = llvm.fdiv %[[FLOAT]], %[[FLOAT]] : f32
// CHECK: {{.*}} = llvm.frem %[[FLOAT]], %[[FLOAT]] : f32
  %8 = llvm.fadd %arg1, %arg1 : f32
  %9 = llvm.fsub %arg1, %arg1 : f32
  %10 = llvm.fmul %arg1, %arg1 : f32
  %11 = llvm.fdiv %arg1, %arg1 : f32
  %12 = llvm.frem %arg1, %arg1 : f32

// Memory-related operations.
//
// CHECK-NEXT:  %[[ALLOCA:.*]] = llvm.alloca %[[I32]] x f64 : (i32) -> !llvm.ptr
// CHECK-NEXT:  %[[GEP:.*]] = llvm.getelementptr %[[ALLOCA]][%[[I32]]] : (!llvm.ptr, i32) -> !llvm.ptr, f64
// CHECK-NEXT:  %[[VALUE:.*]] = llvm.load %[[GEP]] : !llvm.ptr -> f64
// CHECK-NEXT:  llvm.store %[[VALUE]], %[[ALLOCA]] : f64, !llvm.ptr
  %13 = llvm.alloca %arg0 x f64 : (i32) -> !llvm.ptr
  %14 = llvm.getelementptr %13[%arg0] : (!llvm.ptr, i32) -> !llvm.ptr, f64
  %15 = llvm.load %14 : !llvm.ptr -> f64
  llvm.store %15, %13 : f64, !llvm.ptr

// Function call-related operations.
//
// CHECK: %[[STRUCT:.*]] = llvm.call @foo(%[[I32]]) : (i32) -> !llvm.struct<(i32, f64, i32)>
// CHECK: %[[VALUE:.*]] = llvm.extractvalue %[[STRUCT]][0] : !llvm.struct<(i32, f64, i32)>
// CHECK: %[[NEW_STRUCT:.*]] = llvm.insertvalue %[[VALUE]], %[[STRUCT]][2] : !llvm.struct<(i32, f64, i32)>
// CHECK: %[[FUNC:.*]] = llvm.mlir.addressof @foo : !llvm.ptr
// CHECK: %{{.*}} = llvm.call %[[FUNC]](%[[I32]]) : !llvm.ptr, (i32) -> !llvm.struct<(i32, f64, i32)>
  %17 = llvm.call @foo(%arg0) : (i32) -> !llvm.struct<(i32, f64, i32)>
  %18 = llvm.extractvalue %17[0] : !llvm.struct<(i32, f64, i32)>
  %19 = llvm.insertvalue %18, %17[2] : !llvm.struct<(i32, f64, i32)>
  %20 = llvm.mlir.addressof @foo : !llvm.ptr
  %21 = llvm.call %20(%arg0) : !llvm.ptr, (i32) -> !llvm.struct<(i32, f64, i32)>

// Variadic calls
// CHECK:  llvm.call @vararg_func(%arg0, %arg0) vararg(!llvm.func<void (i32, ...)>) : (i32, i32) -> ()
// CHECK:  llvm.call @vararg_func(%arg0, %arg0) vararg(!llvm.func<void (i32, ...)>) {fastmathFlags = #llvm.fastmath<fast>} : (i32, i32) -> ()
// CHECK:  %[[VARIADIC_FUNC:.*]] = llvm.mlir.addressof @vararg_func : !llvm.ptr
// CHECK:  llvm.call %[[VARIADIC_FUNC]](%[[I32]], %[[I32]]) vararg(!llvm.func<void (i32, ...)>) : !llvm.ptr, (i32, i32) -> ()
// CHECK:  llvm.call %[[VARIADIC_FUNC]](%[[I32]], %[[I32]]) vararg(!llvm.func<void (i32, ...)>) {fastmathFlags = #llvm.fastmath<fast>} : !llvm.ptr, (i32, i32) -> ()
  llvm.call @vararg_func(%arg0, %arg0) vararg(!llvm.func<void (i32, ...)>) : (i32, i32) -> ()
  llvm.call @vararg_func(%arg0, %arg0) vararg(!llvm.func<void (i32, ...)>) {fastmathFlags = #llvm.fastmath<fast>} : (i32, i32) -> ()
  %variadic_func = llvm.mlir.addressof @vararg_func : !llvm.ptr
  llvm.call %variadic_func(%arg0, %arg0) vararg(!llvm.func<void (i32, ...)>) : !llvm.ptr, (i32, i32) -> ()
  llvm.call %variadic_func(%arg0, %arg0) vararg(!llvm.func<void (i32, ...)>) {fastmathFlags = #llvm.fastmath<fast>} : !llvm.ptr, (i32, i32) -> ()

// Function call attributes
// CHECK: llvm.call @baz() {convergent} : () -> ()
  llvm.call @baz() {convergent} : () -> ()

// CHECK: llvm.call @baz() {no_unwind} : () -> ()
  llvm.call @baz() {no_unwind} : () -> ()

// CHECK: llvm.call @baz() {will_return} : () -> ()
  llvm.call @baz() {will_return} : () -> ()

// CHECK: llvm.call @baz() {memory = #llvm.memory_effects<other = none, argMem = read, inaccessibleMem = write>} : () -> ()
  llvm.call @baz() {memory = #llvm.memory_effects<other = none, argMem = read, inaccessibleMem = write>} : () -> ()

// Terminator operations and their successors.
//
// CHECK: llvm.br ^[[BB1:.*]]
  llvm.br ^bb1

// CHECK: ^[[BB1]]
^bb1:
// CHECK: llvm.cond_br %7, ^[[BB2:.*]], ^[[BB3:.*]]
  llvm.cond_br %7, ^bb2, ^bb3

// CHECK: ^[[BB2]]
^bb2:
// CHECK: %{{.*}} = llvm.mlir.undef : !llvm.struct<(i32, f64, i32)>
// CHECK: %{{.*}} = llvm.mlir.constant(42 : i64) : i47
  %22 = llvm.mlir.undef : !llvm.struct<(i32, f64, i32)>
  %23 = llvm.mlir.constant(42) : i47
  // CHECK:      llvm.switch %0 : i32, ^[[BB3]] [
  // CHECK-NEXT:   1: ^[[BB4:.*]],
  // CHECK-NEXT:   2: ^[[BB5:.*]],
  // CHECK-NEXT:   3: ^[[BB6:.*]],
  // CHECK-NEXT:   -3: ^[[BB6:.*]]
  // CHECK-NEXT: ]
  llvm.switch %0 : i32, ^bb3 [
    1: ^bb4,
    2: ^bb5,
    3: ^bb6,
    -3: ^bb6
  ]

// CHECK: ^[[BB3]]
^bb3:
// CHECK:      llvm.switch %0 : i32, ^[[BB7:.*]] [
// CHECK-NEXT: ]
  llvm.switch %0 : i32, ^bb7 [
  ]

// CHECK: ^[[BB4]]
^bb4:
  llvm.switch %0 : i32, ^bb7 [
  ]

// CHECK: ^[[BB5]]
^bb5:
  llvm.switch %0 : i32, ^bb7 [
  ]

// CHECK: ^[[BB6]]
^bb6:
  llvm.switch %0 : i32, ^bb7 [
  ]

// CHECK: ^[[BB7]]
^bb7:
// Misc operations.
// CHECK: %{{.*}} = llvm.select %{{.*}}, %{{.*}}, %{{.*}} : i1, i32
  %24 = llvm.select %7, %0, %1 : i1, i32

// Integer to pointer and pointer to integer conversions.
//
// CHECK: %[[PTR:.*]] = llvm.inttoptr %[[I32]] : i32 to !llvm.ptr
// CHECK: %{{.*}} = llvm.ptrtoint %[[PTR]] : !llvm.ptr to i32
  %25 = llvm.inttoptr %arg0 : i32 to !llvm.ptr
  %26 = llvm.ptrtoint %25 : !llvm.ptr to i32

// Extended and Quad floating point
//
// CHECK: %{{.*}} = llvm.fpext %[[FLOAT]] : f32 to f80
// CHECK: %{{.*}} = llvm.fpext %[[FLOAT]] : f32 to f128
  %27 = llvm.fpext %arg1 : f32 to f80
  %28 = llvm.fpext %arg1 : f32 to f128

// CHECK: %{{.*}} = llvm.fneg %[[FLOAT]] : f32
  %29 = llvm.fneg %arg1 : f32

// CHECK: llvm.intr.sin(%[[FLOAT]]) : (f32) -> f32
  %30 = llvm.intr.sin(%arg1) : (f32) -> f32

// CHECK: llvm.intr.pow(%[[FLOAT]], %[[FLOAT]]) : (f32, f32) -> f32
  %31 = llvm.intr.pow(%arg1, %arg1) : (f32, f32) -> f32

// CHECK: llvm.intr.powi(%[[FLOAT]], %[[I32]]) : (f32, i32) -> f32
  %a31 = llvm.intr.powi(%arg1, %arg0) : (f32, i32) -> f32

// CHECK: llvm.intr.bitreverse(%{{.*}}) : (i32) -> i32
  %32 = llvm.intr.bitreverse(%arg0) : (i32) -> i32

// CHECK: llvm.intr.ctpop(%{{.*}}) : (i32) -> i32
  %33 = llvm.intr.ctpop(%arg0) : (i32) -> i32

// CHECK: llvm.intr.round(%[[FLOAT]]) : (f32) -> f32
  %34 = llvm.intr.round(%arg1) : (f32) -> f32

// CHECK: "llvm.intr.memcpy"(%{{.*}}, %{{.*}}, %{{.*}}) <{isVolatile = false}> : (!llvm.ptr, !llvm.ptr, i32) -> ()
  "llvm.intr.memcpy"(%arg2, %arg3, %arg0) <{isVolatile = false}> : (!llvm.ptr, !llvm.ptr, i32) -> ()

// CHECK: "llvm.intr.memcpy"(%{{.*}}, %{{.*}}, %{{.*}}) <{isVolatile = false}> : (!llvm.ptr, !llvm.ptr, i32) -> ()
  "llvm.intr.memcpy"(%arg2, %arg3, %arg0) <{isVolatile = false}> : (!llvm.ptr, !llvm.ptr, i32) -> ()

// CHECK: "llvm.intr.memcpy.inline"(%{{.*}}, %{{.*}}) <{isVolatile = false, len = 10 : i64}> : (!llvm.ptr, !llvm.ptr) -> ()
  "llvm.intr.memcpy.inline"(%arg2, %arg3) <{isVolatile = false, len = 10 : i64}> : (!llvm.ptr, !llvm.ptr) -> ()

// CHECK:  llvm.return
  llvm.return
}

// CHECK-LABEL: @gep
llvm.func @gep(%ptr: !llvm.ptr, %idx: i64, %ptr2: !llvm.ptr) {
  // CHECK: llvm.getelementptr %{{.*}}[%{{.*}}, 1, 0] : (!llvm.ptr, i64) -> !llvm.ptr, !llvm.struct<(i32, struct<(i32, f32)>)>
  llvm.getelementptr %ptr[%idx, 1, 0] : (!llvm.ptr, i64) -> !llvm.ptr, !llvm.struct<(i32, struct<(i32, f32)>)>
  // CHECK: llvm.getelementptr inbounds %{{.*}}[%{{.*}}, 0, %{{.*}}] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.getelementptr inbounds %ptr2[%idx, 0, %idx] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  // CHECK: llvm.getelementptr inbounds|nuw %{{.*}}[%{{.*}}, 0, %{{.*}}] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.getelementptr inbounds | nuw %ptr2[%idx, 0, %idx] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  // CHECK: llvm.getelementptr inbounds %{{.*}}[%{{.*}}, 0, %{{.*}}] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.getelementptr inbounds | nusw %ptr2[%idx, 0, %idx] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  // CHECK: llvm.getelementptr nusw %{{.*}}[%{{.*}}, 0, %{{.*}}] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.getelementptr nusw %ptr2[%idx, 0, %idx] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  // CHECK: llvm.getelementptr nusw|nuw %{{.*}}[%{{.*}}, 0, %{{.*}}] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.getelementptr nusw | nuw %ptr2[%idx, 0, %idx] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  // CHECK: llvm.getelementptr nuw %{{.*}}[%{{.*}}, 0, %{{.*}}] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.getelementptr nuw %ptr2[%idx, 0, %idx] : (!llvm.ptr, i64, i64) -> !llvm.ptr, !llvm.struct<(array<10 x f32>)>
  llvm.return
}

llvm.func @vararg_foo(i32, ...) -> !llvm.struct<(i32, f64, i32)>

// An larger self-contained function.
// CHECK-LABEL: llvm.func @foo(%{{.*}}: i32) -> !llvm.struct<(i32, f64, i32)> {
llvm.func @foo(%arg0: i32) -> !llvm.struct<(i32, f64, i32)> {
// CHECK:  %[[V0:.*]] = llvm.mlir.constant(3 : i64) : i32
// CHECK:  %[[V1:.*]] = llvm.mlir.constant(3 : i64) : i32
// CHECK:  %[[V2:.*]] = llvm.mlir.constant(4.200000e+01 : f64) : f64
// CHECK:  %[[V3:.*]] = llvm.mlir.constant(4.200000e+01 : f64) : f64
// CHECK:  %[[V4:.*]] = llvm.add %[[V0]], %[[V1]] : i32
// CHECK:  %[[V5:.*]] = llvm.mul %[[V4]], %[[V1]] : i32
// CHECK:  %[[V6:.*]] = llvm.fadd %[[V2]], %[[V3]] : f64
// CHECK:  %[[V7:.*]] = llvm.fsub %[[V3]], %[[V6]] : f64
// CHECK:  %[[V8:.*]] = llvm.mlir.constant(1 : i64) : i1
// CHECK:  llvm.cond_br %[[V8]], ^[[BB1:.*]](%[[V4]] : i32), ^[[BB2:.*]](%[[V4]] : i32)
  %0 = llvm.mlir.constant(3) : i32
  %1 = llvm.mlir.constant(3) : i32
  %2 = llvm.mlir.constant(4.200000e+01) : f64
  %3 = llvm.mlir.constant(4.200000e+01) : f64
  %4 = llvm.add %0, %1 : i32
  %5 = llvm.mul %4, %1 : i32
  %6 = llvm.fadd %2, %3 : f64
  %7 = llvm.fsub %3, %6 : f64
  %8 = llvm.mlir.constant(1) : i1
  llvm.cond_br %8, ^bb1(%4 : i32), ^bb2(%4 : i32)

// CHECK:^[[BB1]](%[[V9:.*]]: i32):
// CHECK:  %[[V10:.*]] = llvm.call @foo(%[[V9]]) : (i32) -> !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V11:.*]] = llvm.extractvalue %[[V10]][0] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V12:.*]] = llvm.extractvalue %[[V10]][1] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V13:.*]] = llvm.extractvalue %[[V10]][2] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V14:.*]] = llvm.mlir.undef : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V15:.*]] = llvm.insertvalue %[[V5]], %[[V14]][0] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V16:.*]] = llvm.insertvalue %[[V7]], %[[V15]][1] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V17:.*]] = llvm.insertvalue %[[V11]], %[[V16]][2] : !llvm.struct<(i32, f64, i32)>
// CHECK:  llvm.return %[[V17]] : !llvm.struct<(i32, f64, i32)>
^bb1(%9: i32):
  %10 = llvm.call @foo(%9) : (i32) -> !llvm.struct<(i32, f64, i32)>
  %11 = llvm.extractvalue %10[0] : !llvm.struct<(i32, f64, i32)>
  %12 = llvm.extractvalue %10[1] : !llvm.struct<(i32, f64, i32)>
  %13 = llvm.extractvalue %10[2] : !llvm.struct<(i32, f64, i32)>
  %14 = llvm.mlir.undef : !llvm.struct<(i32, f64, i32)>
  %15 = llvm.insertvalue %5, %14[0] : !llvm.struct<(i32, f64, i32)>
  %16 = llvm.insertvalue %7, %15[1] : !llvm.struct<(i32, f64, i32)>
  %17 = llvm.insertvalue %11, %16[2] : !llvm.struct<(i32, f64, i32)>
  llvm.return %17 : !llvm.struct<(i32, f64, i32)>

// CHECK:^[[BB2]](%[[V18:.*]]: i32):
// CHECK:  %[[V19:.*]] = llvm.mlir.undef : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V20:.*]] = llvm.insertvalue %[[V18]], %[[V19]][0] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V21:.*]] = llvm.insertvalue %[[V7]], %[[V20]][1] : !llvm.struct<(i32, f64, i32)>
// CHECK:  %[[V22:.*]] = llvm.insertvalue %[[V5]], %[[V21]][2] : !llvm.struct<(i32, f64, i32)>
// CHECK:  llvm.return %[[V22]] : !llvm.struct<(i32, f64, i32)>
^bb2(%18: i32):
  %19 = llvm.mlir.undef : !llvm.struct<(i32, f64, i32)>
  %20 = llvm.insertvalue %18, %19[0] : !llvm.struct<(i32, f64, i32)>
  %21 = llvm.insertvalue %7, %20[1] : !llvm.struct<(i32, f64, i32)>
  %22 = llvm.insertvalue %5, %21[2] : !llvm.struct<(i32, f64, i32)>
  llvm.return %22 : !llvm.struct<(i32, f64, i32)>
}

// CHECK-LABEL: @casts
// CHECK-SAME: (%[[I32:.*]]: i32, %[[I64:.*]]: i64, %[[V4I32:.*]]: vector<4xi32>, %[[V4I64:.*]]: vector<4xi64>, %[[PTR:.*]]: !llvm.ptr)
func.func @casts(%arg0: i32, %arg1: i64, %arg2: vector<4xi32>,
            %arg3: vector<4xi64>, %arg4: !llvm.ptr) {
// CHECK:  = llvm.sext %[[I32]] : i32 to i56
  %0 = llvm.sext %arg0 : i32 to i56
// CHECK:  = llvm.zext %[[I32]] : i32 to i64
  %1 = llvm.zext %arg0 : i32 to i64
// CHECK:  = llvm.trunc %[[I64]] : i64 to i56
  %2 = llvm.trunc %arg1 : i64 to i56
// CHECK:  = llvm.sext %[[V4I32]] : vector<4xi32> to vector<4xi56>
  %3 = llvm.sext %arg2 : vector<4xi32> to vector<4xi56>
// CHECK:  = llvm.zext %[[V4I32]] : vector<4xi32> to vector<4xi64>
  %4 = llvm.zext %arg2 : vector<4xi32> to vector<4xi64>
// CHECK:  = llvm.trunc %[[V4I64]] : vector<4xi64> to vector<4xi56>
  %5 = llvm.trunc %arg3 : vector<4xi64> to vector<4xi56>
// CHECK:  = llvm.sitofp %[[I32]] : i32 to f32
  %6 = llvm.sitofp %arg0 : i32 to f32
// CHECK: %[[FLOAT:.*]] = llvm.uitofp %[[I32]] : i32 to f32
  %7 = llvm.uitofp %arg0 : i32 to f32
// CHECK:  = llvm.fptosi %[[FLOAT]] : f32 to i32
  %8 = llvm.fptosi %7 : f32 to i32
// CHECK:  = llvm.fptoui %[[FLOAT]] : f32 to i32
  %9 = llvm.fptoui %7 : f32 to i32
// CHECK:  = llvm.addrspacecast %[[PTR]] : !llvm.ptr to !llvm.ptr<2>
  %10 = llvm.addrspacecast %arg4 : !llvm.ptr to !llvm.ptr<2>
// CHECK:  = llvm.bitcast %[[I64]] : i64 to f64
  %11 = llvm.bitcast %arg1 : i64 to f64
  llvm.return
}

// CHECK-LABEL: @nneg_casts
// CHECK-SAME: (%[[I32:.*]]: i32, %[[I64:.*]]: i64, %[[V4I32:.*]]: vector<4xi32>, %[[V4I64:.*]]: vector<4xi64>, %[[PTR:.*]]: !llvm.ptr)
func.func @nneg_casts(%arg0: i32, %arg1: i64, %arg2: vector<4xi32>,
                %arg3: vector<4xi64>, %arg4: !llvm.ptr) {
// CHECK:  = llvm.zext nneg %[[I32]] : i32 to i64
  %0 = llvm.zext nneg %arg0 : i32 to i64
// CHECK:  = llvm.zext nneg %[[V4I32]] : vector<4xi32> to vector<4xi64>
  %4 = llvm.zext nneg %arg2 : vector<4xi32> to vector<4xi64>
// CHECK:  = llvm.uitofp nneg %[[I32]] : i32 to f32
  %7 = llvm.uitofp nneg %arg0 : i32 to f32
  llvm.return
}

// CHECK-LABEL: @casts_overflow
// CHECK-SAME: (%[[I32:.*]]: i32, %[[I64:.*]]: i64, %[[V4I32:.*]]: vector<4xi32>, %[[V4I64:.*]]: vector<4xi64>, %[[PTR:.*]]: !llvm.ptr)
func.func @casts_overflow(%arg0: i32, %arg1: i64, %arg2: vector<4xi32>,
            %arg3: vector<4xi64>, %arg4: !llvm.ptr) {
// CHECK:  = llvm.trunc %[[I64]] overflow<nsw> : i64 to i56
  %0 = llvm.trunc %arg1 overflow<nsw> : i64 to i56
// CHECK:  = llvm.trunc %[[I64]] overflow<nuw> : i64 to i56
  %1 = llvm.trunc %arg1 overflow<nuw> : i64 to i56
// CHECK:  = llvm.trunc %[[I64]] overflow<nsw, nuw> : i64 to i56
  %2 = llvm.trunc %arg1 overflow<nsw, nuw> : i64 to i56
// CHECK:  = llvm.trunc %[[I64]] overflow<nsw, nuw> : i64 to i56
  %3 = llvm.trunc %arg1 overflow<nuw, nsw> : i64 to i56
// CHECK:  = llvm.trunc %[[V4I64]] overflow<nsw> : vector<4xi64> to vector<4xi56>
  %4 = llvm.trunc %arg3 overflow<nsw> : vector<4xi64> to vector<4xi56>
  llvm.return
}

// CHECK-LABEL: @vect
func.func @vect(%arg0: vector<4xf32>, %arg1: i32, %arg2: f32, %arg3: vector<2x!llvm.ptr>) {
// CHECK:  = llvm.extractelement {{.*}} : vector<4xf32>
  %0 = llvm.extractelement %arg0[%arg1 : i32] : vector<4xf32>
// CHECK:  = llvm.insertelement {{.*}} : vector<4xf32>
  %1 = llvm.insertelement %arg2, %arg0[%arg1 : i32] : vector<4xf32>
// CHECK:  = llvm.shufflevector {{.*}} [0, 0, 0, 0, 7] : vector<4xf32>
  %2 = llvm.shufflevector %arg0, %arg0 [0, 0, 0, 0, 7] : vector<4xf32>
// CHECK:  = llvm.shufflevector %{{.+}}, %{{.+}} [1, 0] : vector<2x!llvm.ptr>
  %3 = llvm.shufflevector %arg3, %arg3 [1, 0] : vector<2x!llvm.ptr>
// CHECK:  = llvm.mlir.constant(dense<1.000000e+00> : vector<4xf32>) : vector<4xf32>
  %4 = llvm.mlir.constant(dense<1.0> : vector<4xf32>) : vector<4xf32>
  return
}

// CHECK-LABEL: @scalable_vect
func.func @scalable_vect(%arg0: vector<[4]xf32>, %arg1: i32, %arg2: f32) {
// CHECK:  = llvm.extractelement {{.*}} : vector<[4]xf32>
  %0 = llvm.extractelement %arg0[%arg1 : i32] : vector<[4]xf32>
// CHECK:  = llvm.insertelement {{.*}} : vector<[4]xf32>
  %1 = llvm.insertelement %arg2, %arg0[%arg1 : i32] : vector<[4]xf32>
// CHECK:  = llvm.shufflevector {{.*}} [0, 0, 0, 0] : vector<[4]xf32>
  %2 = llvm.shufflevector %arg0, %arg0 [0, 0, 0, 0] : vector<[4]xf32>
// CHECK:  = llvm.mlir.constant(dense<1.000000e+00> : vector<[4]xf32>) : vector<[4]xf32>
  %3 = llvm.mlir.constant(dense<1.0> : vector<[4]xf32>) : vector<[4]xf32>
  return
}

// CHECK-LABEL: @mixed_vect
func.func @mixed_vect(%arg0: vector<8xf32>, %arg1: vector<4xf32>, %arg2: vector<[4]xf32>) {
  // CHECK: = llvm.intr.vector.insert {{.*}} : vector<8xf32> into vector<[4]xf32>
  %0 = llvm.intr.vector.insert %arg0, %arg2[0] : vector<8xf32> into vector<[4]xf32>
  // CHECK: = llvm.intr.vector.insert {{.*}} : vector<4xf32> into vector<[4]xf32>
  %1 = llvm.intr.vector.insert %arg1, %arg2[0] : vector<4xf32> into vector<[4]xf32>
  // CHECK: = llvm.intr.vector.insert {{.*}} : vector<4xf32> into vector<[4]xf32>
  %2 = llvm.intr.vector.insert %arg1, %1[4] : vector<4xf32> into vector<[4]xf32>
  // CHECK: = llvm.intr.vector.insert {{.*}} : vector<4xf32> into vector<8xf32>
  %3 = llvm.intr.vector.insert %arg1, %arg0[4] : vector<4xf32> into vector<8xf32>
  // CHECK: = llvm.intr.vector.extract {{.*}} : vector<8xf32> from vector<[4]xf32>
  %4 = llvm.intr.vector.extract %2[0] : vector<8xf32> from vector<[4]xf32>
  // CHECK: = llvm.intr.vector.extract {{.*}} : vector<2xf32> from vector<8xf32>
  %5 = llvm.intr.vector.extract %arg0[6] : vector<2xf32> from vector<8xf32>
  return
}

// CHECK-LABEL: @vector_interleave2
func.func @vector_interleave2(%vec1: vector<[4]xf16>, %vec2 : vector<[4]xf16>) {
  // CHECK: = "llvm.intr.vector.interleave2"({{.*}}) : (vector<[4]xf16>, vector<[4]xf16>) -> vector<[8]xf16>
  %0 = "llvm.intr.vector.interleave2"(%vec1, %vec2) : (vector<[4]xf16>, vector<[4]xf16>) -> vector<[8]xf16>
  return
}

// CHECK-LABEL: @vector_deinterleave2
func.func @vector_deinterleave2(%vec: vector<[8]xf16>) {
  // CHECK: = "llvm.intr.vector.deinterleave2"({{.*}}) : (vector<[8]xf16>) -> !llvm.struct<(vector<[4]xf16>, vector<[4]xf16>)>
  %0 = "llvm.intr.vector.deinterleave2"(%vec) : (vector<[8]xf16>) -> !llvm.struct<(vector<[4]xf16>, vector<[4]xf16>)>
  return
}

// CHECK-LABEL: @alloca
func.func @alloca(%size : i64) {
  // CHECK: llvm.alloca %{{.*}} x i32 : (i64) -> !llvm.ptr
  llvm.alloca %size x i32 {alignment = 0} : (i64) -> (!llvm.ptr)
  // CHECK: llvm.alloca inalloca %{{.*}} x i32 {alignment = 8 : i64} : (i64) -> !llvm.ptr
  llvm.alloca inalloca %size x i32 {alignment = 8} : (i64) -> (!llvm.ptr)
  llvm.return
}

// CHECK-LABEL: @null
func.func @null() {
  // CHECK: llvm.mlir.zero : !llvm.ptr
  %0 = llvm.mlir.zero : !llvm.ptr
  llvm.return
}

// CHECK-LABEL: @zero
func.func @zero() {
  // CHECK: llvm.mlir.zero : i8
  %0 = llvm.mlir.zero : i8
  llvm.return
}

// CHECK-LABEL: @atomic_load
func.func @atomic_load(%ptr : !llvm.ptr) {
  // CHECK: llvm.load %{{.*}} atomic monotonic {alignment = 4 : i64} : !llvm.ptr -> f32
  %0 = llvm.load %ptr atomic monotonic {alignment = 4 : i64} : !llvm.ptr -> f32
  // CHECK: llvm.load volatile %{{.*}} atomic syncscope("singlethread") monotonic {alignment = 16 : i64} : !llvm.ptr -> f32
  %1 = llvm.load volatile %ptr atomic syncscope("singlethread") monotonic {alignment = 16 : i64} : !llvm.ptr -> f32
  // CHECK: llvm.load %{{.*}} atomic monotonic {alignment = 4 : i64} : !llvm.ptr -> i128
  %2 = llvm.load %ptr atomic monotonic {alignment = 4 : i64} : !llvm.ptr -> i128
  llvm.return
}

// CHECK-LABEL: @atomic_store
func.func @atomic_store(%val : f32, %large_val : i256, %ptr : !llvm.ptr) {
  // CHECK: llvm.store %{{.*}}, %{{.*}} atomic monotonic {alignment = 4 : i64} : f32, !llvm.ptr
  llvm.store %val, %ptr atomic monotonic {alignment = 4 : i64} : f32, !llvm.ptr
  // CHECK: llvm.store volatile %{{.*}}, %{{.*}} atomic syncscope("singlethread") monotonic {alignment = 16 : i64} : f32, !llvm.ptr
  llvm.store volatile %val, %ptr atomic syncscope("singlethread") monotonic {alignment = 16 : i64} : f32, !llvm.ptr
  // CHECK: llvm.store %{{.*}}, %{{.*}} atomic monotonic {alignment = 4 : i64} : i256, !llvm.ptr
  llvm.store %large_val, %ptr atomic monotonic {alignment = 4 : i64} : i256, !llvm.ptr
  llvm.return
}

// CHECK-LABEL: @atomicrmw
func.func @atomicrmw(%ptr : !llvm.ptr, %f32 : f32, %f16_vec : vector<2xf16>) {
  // CHECK: llvm.atomicrmw fadd %{{.*}}, %{{.*}} monotonic : !llvm.ptr, f32
  %0 = llvm.atomicrmw fadd %ptr, %f32 monotonic : !llvm.ptr, f32
  // CHECK: llvm.atomicrmw volatile fsub %{{.*}}, %{{.*}} syncscope("singlethread") monotonic {alignment = 16 : i64} : !llvm.ptr, f32
  %1 = llvm.atomicrmw volatile fsub %ptr, %f32 syncscope("singlethread") monotonic {alignment = 16 : i64} : !llvm.ptr, f32
  // CHECK: llvm.atomicrmw fmin %{{.*}}, %{{.*}} monotonic : !llvm.ptr, vector<2xf16>
  %2 = llvm.atomicrmw fmin %ptr, %f16_vec monotonic : !llvm.ptr, vector<2xf16>
  llvm.return
}

// CHECK-LABEL: @cmpxchg
func.func @cmpxchg(%ptr : !llvm.ptr, %cmp : i32, %new : i32) {
  // CHECK: llvm.cmpxchg %{{.*}}, %{{.*}}, %{{.*}} acq_rel monotonic : !llvm.ptr, i32
  %0 = llvm.cmpxchg %ptr, %cmp, %new acq_rel monotonic : !llvm.ptr, i32
  // CHECK: llvm.cmpxchg weak volatile %{{.*}}, %{{.*}}, %{{.*}} syncscope("singlethread") acq_rel monotonic {alignment = 16 : i64} : !llvm.ptr, i32
  %1 = llvm.cmpxchg weak volatile %ptr, %cmp, %new syncscope("singlethread") acq_rel monotonic {alignment = 16 : i64} : !llvm.ptr, i32
  llvm.return
}

// CHECK-LABEL: @invariant_load
func.func @invariant_load(%ptr : !llvm.ptr) -> i32 {
  // CHECK: llvm.load %{{.+}} invariant {alignment = 4 : i64} : !llvm.ptr -> i32
  %0 = llvm.load %ptr invariant {alignment = 4 : i64} : !llvm.ptr -> i32
  func.return %0 : i32
}

// CHECK-LABEL: @invariant_group_load
func.func @invariant_group_load(%ptr : !llvm.ptr) -> i32 {
  // CHECK: llvm.load %{{.+}} invariant_group {alignment = 4 : i64} : !llvm.ptr -> i32
  %0 = llvm.load %ptr invariant_group {alignment = 4 : i64} : !llvm.ptr -> i32
  func.return %0 : i32
}

// CHECK-LABEL: @invariant_group_store
func.func @invariant_group_store(%val: i32, %ptr : !llvm.ptr) {
  // CHECK: llvm.store %{{.+}}, %{{.+}} invariant_group : i32, !llvm.ptr
  llvm.store %val, %ptr invariant_group : i32, !llvm.ptr
  func.return
}

llvm.mlir.global external constant @_ZTIi() : !llvm.ptr
llvm.func @bar(!llvm.ptr, !llvm.ptr, !llvm.ptr)
llvm.func @__gxx_personality_v0(...) -> i32

// CHECK-LABEL: @invokeLandingpad
llvm.func @invokeLandingpad() -> i32 attributes { personality = @__gxx_personality_v0 } {
// CHECK: %[[V0:.*]] = llvm.mlir.constant(0 : i32) : i32
// CHECK: %{{.*}} = llvm.mlir.constant(3 : i32) : i32
// CHECK: %[[V1:.*]] = llvm.mlir.constant("\01") : !llvm.array<1 x i8>
// CHECK: %[[V2:.*]] = llvm.mlir.zero : !llvm.ptr
// CHECK: %[[V3:.*]] = llvm.mlir.addressof @_ZTIi : !llvm.ptr
// CHECK: %[[V4:.*]] = llvm.mlir.constant(1 : i32) : i32
// CHECK: %[[V5:.*]] = llvm.alloca %[[V4]] x i8 : (i32) -> !llvm.ptr
// CHECK: %{{.*}} = llvm.invoke @foo(%[[V4]]) to ^[[BB2:.*]] unwind ^[[BB1:.*]] : (i32) -> !llvm.struct<(i32, f64, i32)>
  %0 = llvm.mlir.constant(0 : i32) : i32
  %1 = llvm.mlir.constant(3 : i32) : i32
  %2 = llvm.mlir.constant("\01") : !llvm.array<1 x i8>
  %3 = llvm.mlir.zero : !llvm.ptr
  %4 = llvm.mlir.addressof @_ZTIi : !llvm.ptr
  %5 = llvm.mlir.constant(1 : i32) : i32
  %6 = llvm.alloca %5 x i8 : (i32) -> !llvm.ptr
  %7 = llvm.invoke @foo(%5) to ^bb2 unwind ^bb1 : (i32) -> !llvm.struct<(i32, f64, i32)>

// CHECK: ^[[BB1]]:
// CHECK:   %[[lp:.*]] = llvm.landingpad cleanup (catch %[[V2]] : !llvm.ptr) (catch %[[V3]] : !llvm.ptr) (filter %[[V1]] : !llvm.array<1 x i8>) : !llvm.struct<(ptr, i32)>
// CHECK:   %{{.*}} = llvm.intr.eh.typeid.for %[[V3]] : (!llvm.ptr) -> i32
// CHECK:   llvm.resume %[[lp]] : !llvm.struct<(ptr, i32)>
^bb1:
  %10 = llvm.landingpad cleanup (catch %3 : !llvm.ptr) (catch %4 : !llvm.ptr) (filter %2 : !llvm.array<1 x i8>) : !llvm.struct<(ptr, i32)>
  %11 = llvm.intr.eh.typeid.for %4 : (!llvm.ptr) -> i32
  llvm.resume %10 : !llvm.struct<(ptr, i32)>

// CHECK: ^[[BB2]]:
// CHECK:   llvm.return %[[V4]] : i32
^bb2:
  llvm.return %5 : i32

// CHECK: ^[[BB3:.*]]:
// CHECK:   llvm.invoke @bar(%[[V5]], %[[V3]], %[[V2]]) to ^[[BB2]] unwind ^[[BB1]] : (!llvm.ptr, !llvm.ptr, !llvm.ptr) -> ()
^bb3:
  llvm.invoke @bar(%6, %4, %3) to ^bb2 unwind ^bb1 : (!llvm.ptr, !llvm.ptr, !llvm.ptr) -> ()

// CHECK: ^[[BB4:.*]]:
// CHECK: %[[FUNC:.*]] = llvm.mlir.addressof @foo : !llvm.ptr
// CHECK: %{{.*}} = llvm.invoke %[[FUNC]]{{.*}}: !llvm.ptr,
^bb4:
  %12 = llvm.mlir.addressof @foo : !llvm.ptr
  %13 = llvm.invoke %12(%5) to ^bb2 unwind ^bb1 : !llvm.ptr, (i32) -> !llvm.struct<(i32, f64, i32)>

// CHECK: ^[[BB5:.*]]:
// CHECK: %{{.*}} = llvm.invoke @{{.*}} vararg(!llvm.func<struct<(i32, f64, i32)> (i32, ...)>) : (i32, i32) -> !llvm.struct<(i32, f64, i32)>

^bb5:
  %14 = llvm.invoke @vararg_foo(%5, %5) to ^bb2 unwind ^bb1 vararg(!llvm.func<struct<(i32, f64, i32)> (i32, ...)>) : (i32, i32) -> !llvm.struct<(i32, f64, i32)>

// CHECK: ^[[BB6:.*]]:
// CHECK: %[[FUNC:.*]] = llvm.mlir.addressof @vararg_foo : !llvm.ptr
// CHECK: %{{.*}} = llvm.invoke %[[FUNC]]{{.*}} vararg(!llvm.func<struct<(i32, f64, i32)> (i32, ...)>) : !llvm.ptr, (i32, i32) -> !llvm.struct<(i32, f64, i32)>
^bb6:
  %15 = llvm.mlir.addressof @vararg_foo : !llvm.ptr
  %16 = llvm.invoke %15(%5, %5) to ^bb2 unwind ^bb1 vararg(!llvm.func<!llvm.struct<(i32, f64, i32)> (i32, ...)>) : !llvm.ptr, (i32, i32) -> !llvm.struct<(i32, f64, i32)>

// CHECK: ^[[BB7:.*]]:
// CHECK:   llvm.return %[[V0]] : i32
^bb7:
  llvm.return %0 : i32
}

// CHECK-LABEL: @useFreezeOp
func.func @useFreezeOp(%arg0: i32) {
  // CHECK:  = llvm.freeze %[[ARG0:.*]] : i32
  %0 = llvm.freeze %arg0 : i32
  // CHECK: %[[UNDEF:.*]] = llvm.mlir.undef : i8
  %1 = llvm.mlir.undef : i8
  // CHECK:  = llvm.freeze %[[UNDEF]] : i8
  %2 = llvm.freeze %1 : i8
  // CHECK: %[[POISON:.*]] = llvm.mlir.poison : i8
  %3 = llvm.mlir.poison : i8
  // CHECK:  = llvm.freeze %[[POISON]] : i8
  %4 = llvm.freeze %3 : i8
  return
}

// CHECK-LABEL: @useFenceInst
func.func @useFenceInst() {
  // CHECK:  syncscope("agent") seq_cst
  llvm.fence syncscope("agent") seq_cst
  // CHECK:  seq_cst
  llvm.fence syncscope("") seq_cst
  // CHECK:  release
  llvm.fence release
  return
}

// CHECK-LABEL: @useInlineAsm
llvm.func @useInlineAsm(%arg0: i32) {
  //      CHECK:  llvm.inline_asm {{.*}} (i32) -> i8
  %0 = llvm.inline_asm "bswap $0", "=r,r" %arg0 : (i32) -> i8

  // CHECK-NEXT:  llvm.inline_asm {{.*}} (i32, i32) -> i8
  %1 = llvm.inline_asm "foo", "bar" %arg0, %arg0 : (i32, i32) -> i8

  // CHECK-NEXT:  llvm.inline_asm has_side_effects {{.*}} (i32, i32) -> i8
  %2 = llvm.inline_asm has_side_effects "foo", "bar" %arg0, %arg0 : (i32, i32) -> i8

  // CHECK-NEXT:  llvm.inline_asm is_align_stack {{.*}} (i32, i32) -> i8
  %3 = llvm.inline_asm is_align_stack "foo", "bar" %arg0, %arg0 : (i32, i32) -> i8

  // CHECK-NEXT:  llvm.inline_asm "foo", "=r,=r,r" {{.*}} : (i32) -> !llvm.struct<(i8, i8)>
  %5 = llvm.inline_asm "foo", "=r,=r,r" %arg0 : (i32) -> !llvm.struct<(i8, i8)>

  llvm.return
}

// CHECK-LABEL: @fastmathFlags
func.func @fastmathFlags(%arg0: f32, %arg1: f32, %arg2: i32, %arg3: vector<2 x f32>, %arg4: vector<2 x f32>) {
// CHECK: {{.*}} = llvm.fadd %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
// CHECK: {{.*}} = llvm.fsub %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
// CHECK: {{.*}} = llvm.fmul %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
// CHECK: {{.*}} = llvm.fdiv %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
// CHECK: {{.*}} = llvm.frem %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %0 = llvm.fadd %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %1 = llvm.fsub %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %2 = llvm.fmul %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %3 = llvm.fdiv %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %4 = llvm.frem %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32

// CHECK: %[[SCALAR_PRED0:.+]] = llvm.fcmp "oeq" %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %5 = llvm.fcmp "oeq" %arg0, %arg1 {fastmathFlags = #llvm.fastmath<fast>} : f32
// CHECK: %{{.*}} = llvm.add %[[SCALAR_PRED0]], %[[SCALAR_PRED0]] : i1
  %typecheck_5 = llvm.add %5, %5 : i1
// CHECK: %[[VEC_PRED0:.+]] = llvm.fcmp "oeq" %arg3, %arg4 {fastmathFlags = #llvm.fastmath<fast>} : vector<2xf32>
  %vcmp = llvm.fcmp "oeq" %arg3, %arg4 {fastmathFlags = #llvm.fastmath<fast>} : vector<2xf32>
// CHECK: %{{.*}} = llvm.add %[[VEC_PRED0]], %[[VEC_PRED0]] : vector<2xi1>
  %typecheck_vcmp = llvm.add %vcmp, %vcmp : vector<2xi1>

// CHECK: {{.*}} = llvm.fneg %arg0 {fastmathFlags = #llvm.fastmath<fast>} : f32
  %6 = llvm.fneg %arg0 {fastmathFlags = #llvm.fastmath<fast>} : f32

// CHECK: {{.*}} = llvm.call @foo(%arg2) {fastmathFlags = #llvm.fastmath<fast>} : (i32) -> !llvm.struct<(i32, f64, i32)>
  %7 = llvm.call @foo(%arg2) {fastmathFlags = #llvm.fastmath<fast>} : (i32) -> !llvm.struct<(i32, f64, i32)>

// CHECK: {{.*}} = llvm.fadd %arg0, %arg1 : f32
  %8 = llvm.fadd %arg0, %arg1 {fastmathFlags = #llvm.fastmath<none>} : f32
// CHECK: {{.*}} = llvm.fadd %arg0, %arg1 {fastmathFlags = #llvm.fastmath<nnan, ninf>} : f32
  %9 = llvm.fadd %arg0, %arg1 {fastmathFlags = #llvm.fastmath<nnan,ninf>} : f32

// CHECK: {{.*}} = llvm.fneg %arg0 : f32
  %10 = llvm.fneg %arg0 {fastmathFlags = #llvm.fastmath<none>} : f32

// CHECK: {{.*}} = llvm.intr.sin(%arg0) {fastmathFlags = #llvm.fastmath<fast>} : (f32) -> f32
  %11 = llvm.intr.sin(%arg0) {fastmathFlags = #llvm.fastmath<fast>} : (f32) -> f32
// CHECK: {{.*}} = llvm.intr.sin(%arg0) {fastmathFlags = #llvm.fastmath<afn>} : (f32) -> f32
  %12 = llvm.intr.sin(%arg0) {fastmathFlags = #llvm.fastmath<afn>} : (f32) -> f32

// CHECK: {{.*}} = llvm.intr.vector.reduce.fmin(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
  %13 = llvm.intr.vector.reduce.fmin(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
// CHECK: {{.*}} = llvm.intr.vector.reduce.fmax(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
  %14 = llvm.intr.vector.reduce.fmax(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
// CHECK: {{.*}} = llvm.intr.vector.reduce.fminimum(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
  %15 = llvm.intr.vector.reduce.fminimum(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
// CHECK: {{.*}} = llvm.intr.vector.reduce.fmaximum(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
  %16 = llvm.intr.vector.reduce.fmaximum(%arg3) {fastmathFlags = #llvm.fastmath<nnan>} : (vector<2xf32>) -> f32
  return
}

// CHECK-LABEL: @lifetime
// CHECK-SAME: %[[P:.*]]: !llvm.ptr
llvm.func @lifetime(%p: !llvm.ptr) {
  // CHECK: llvm.intr.lifetime.start 16, %[[P]]
  llvm.intr.lifetime.start 16, %p : !llvm.ptr
  // CHECK: llvm.intr.lifetime.end 16, %[[P]]
  llvm.intr.lifetime.end 16, %p : !llvm.ptr
  llvm.return
}

// CHECK-LABEL: @invariant
// CHECK-SAME: %[[P:.*]]: !llvm.ptr
llvm.func @invariant(%p: !llvm.ptr) {
  // CHECK: %[[START:.*]] = llvm.intr.invariant.start 1, %[[P]] : !llvm.ptr
  %1 = llvm.intr.invariant.start 1, %p : !llvm.ptr
  // CHECK: llvm.intr.invariant.end %[[START]], 1, %[[P]] : !llvm.ptr
  llvm.intr.invariant.end %1, 1, %p : !llvm.ptr
  llvm.return
}

// CHECK-LABEL: @invariant_group_intrinsics
// CHECK-SAME: %[[P:.+]]: !llvm.ptr
llvm.func @invariant_group_intrinsics(%p: !llvm.ptr) {
  // CHECK: %{{.+}} = llvm.intr.launder.invariant.group %[[P]] : !llvm.ptr
  %1 = llvm.intr.launder.invariant.group %p : !llvm.ptr
  // CHECK: %{{.+}} = llvm.intr.strip.invariant.group %[[P]] : !llvm.ptr
  %2 = llvm.intr.strip.invariant.group %p : !llvm.ptr
  llvm.return
}

// CHECK-LABEL: @vararg_func
llvm.func @vararg_func(%arg0: i32, ...) {
  // CHECK: %[[C:.*]] = llvm.mlir.constant(1 : i32)
  // CHECK: %[[LIST:.*]] = llvm.alloca
  // CHECK: llvm.intr.vastart %[[LIST]] : !llvm.ptr{{$}}
  %1 = llvm.mlir.constant(1 : i32) : i32
  %list = llvm.alloca %1 x !llvm.struct<"struct.va_list_opaque", (ptr)> : (i32) -> !llvm.ptr
  llvm.intr.vastart %list : !llvm.ptr

  // CHECK: %[[LIST2:.*]] = llvm.alloca
  // CHECK: llvm.intr.vacopy %[[LIST]] to %[[LIST2]] : !llvm.ptr, !llvm.ptr{{$}}
  %list2 = llvm.alloca %1 x !llvm.struct<"struct.va_list_opaque", (ptr)> : (i32) -> !llvm.ptr
  llvm.intr.vacopy %list to %list2 : !llvm.ptr, !llvm.ptr

  // CHECK: %[[RET:.+]] = llvm.va_arg %[[LIST2]] : (!llvm.ptr) -> i32
  %ret = llvm.va_arg %list2 : (!llvm.ptr) -> i32

  // CHECK: llvm.intr.vaend %[[LIST]] : !llvm.ptr{{$}}
  // CHECK: llvm.intr.vaend %[[LIST2]] : !llvm.ptr{{$}}
  llvm.intr.vaend %list : !llvm.ptr
  llvm.intr.vaend %list2 : !llvm.ptr
  llvm.return
}

// CHECK-LABEL: @eh_typeid
// CHECK-SAME: %[[ARG0:.*]]: !llvm.ptr
llvm.func @eh_typeid(%arg0: !llvm.ptr) -> i32 {
  // CHECK: llvm.intr.eh.typeid.for %[[ARG0]] : (!llvm.ptr) -> i32
  %0 = llvm.intr.eh.typeid.for %arg0 : (!llvm.ptr) -> i32
  llvm.return %0 : i32
}

// CHECK-LABEL: @stackrestore
// CHECK-SAME: %[[ARG0:.*]]: !llvm.ptr
llvm.func @stackrestore(%arg0: !llvm.ptr)  {
  // CHECK: llvm.intr.stackrestore %[[ARG0]] : !llvm.ptr
  llvm.intr.stackrestore %arg0 : !llvm.ptr
  llvm.return
}

#alias_scope_domain = #llvm.alias_scope_domain<id = distinct[0]<>, description = "The domain">
#alias_scope = #llvm.alias_scope<id = distinct[0]<>, domain = #alias_scope_domain, description = "The domain">

// CHECK-LABEL: @experimental_noalias_scope_decl
llvm.func @experimental_noalias_scope_decl() {
  // CHECK: llvm.intr.experimental.noalias.scope.decl #{{.*}}
  llvm.intr.experimental.noalias.scope.decl #alias_scope
  llvm.return
}

#alias_scope_domain2 = #llvm.alias_scope_domain<id = "domainid", description = "The domain">
#alias_scope2 = #llvm.alias_scope<id = "stringid", domain = #alias_scope_domain2, description = "The domain">

// CHECK-LABEL: @experimental_noalias_scope_with_string_id
llvm.func @experimental_noalias_scope_with_string_id() {
  // CHECK: llvm.intr.experimental.noalias.scope.decl #{{.*}}
  llvm.intr.experimental.noalias.scope.decl #alias_scope2
  llvm.return
}

// CHECK-LABEL: @experimental_constrained_fptrunc
llvm.func @experimental_constrained_fptrunc(%in: f64) {
  // CHECK: llvm.intr.experimental.constrained.fptrunc %{{.*}} towardzero ignore : f64 to f32
  %0 = llvm.intr.experimental.constrained.fptrunc %in towardzero ignore : f64 to f32
  // CHECK: llvm.intr.experimental.constrained.fptrunc %{{.*}} tonearest maytrap : f64 to f32
  %1 = llvm.intr.experimental.constrained.fptrunc %in tonearest maytrap : f64 to f32
  // CHECK: llvm.intr.experimental.constrained.fptrunc %{{.*}} upward strict : f64 to f32
  %2 = llvm.intr.experimental.constrained.fptrunc %in upward strict : f64 to f32
  // CHECK: llvm.intr.experimental.constrained.fptrunc %{{.*}} downward ignore : f64 to f32
  %3 = llvm.intr.experimental.constrained.fptrunc %in downward ignore : f64 to f32
  // CHECK: llvm.intr.experimental.constrained.fptrunc %{{.*}} tonearestaway ignore : f64 to f32
  %4 = llvm.intr.experimental.constrained.fptrunc %in tonearestaway ignore : f64 to f32
  llvm.return
}

// CHECK: llvm.func @tail_call_target() -> i32
llvm.func @tail_call_target() -> i32

// CHECK-LABEL: @test_none
llvm.func @test_none() -> i32 {
  // CHECK-NEXT: llvm.call @tail_call_target() : () -> i32
  %0 = llvm.call none @tail_call_target() : () -> i32
  llvm.return %0 : i32
}

// CHECK-LABEL: @test_default
llvm.func @test_default() -> i32 {
  // CHECK-NEXT: llvm.call @tail_call_target() : () -> i32
  %0 = llvm.call @tail_call_target() : () -> i32
  llvm.return %0 : i32
}

// CHECK-LABEL: @test_musttail
llvm.func @test_musttail() -> i32 {
  // CHECK-NEXT: llvm.call musttail @tail_call_target() : () -> i32
  %0 = llvm.call musttail @tail_call_target() : () -> i32
  llvm.return %0 : i32
}

// CHECK-LABEL: @test_tail
llvm.func @test_tail() -> i32 {
  // CHECK-NEXT: llvm.call tail @tail_call_target() : () -> i32
  %0 = llvm.call tail @tail_call_target() : () -> i32
  llvm.return %0 : i32
}

// CHECK-LABEL: @test_notail
llvm.func @test_notail() -> i32 {
  // CHECK-NEXT: llvm.call notail @tail_call_target() : () -> i32
  %0 = llvm.call notail @tail_call_target() : () -> i32
  llvm.return %0 : i32
}

// CHECK-LABEL: @vector_predication_intrinsics
// CHECK-SAME: (%[[ARG0:.*]]: vector<8xi32>, %[[ARG1:.*]]: vector<8xi32>, %[[ARG2:.*]]: vector<8xi1>, %[[ARG3:.*]]: i32)
llvm.func @vector_predication_intrinsics(%A: vector<8xi32>, %B: vector<8xi32>,
                                         %mask: vector<8xi1>, %evl: i32) {
  // CHECK-NEXT: "llvm.intr.vp.smax"(%[[ARG0]], %[[ARG1]], %[[ARG2]], %[[ARG3]])
  "llvm.intr.vp.smax" (%A, %B, %mask, %evl) :
         (vector<8xi32>, vector<8xi32>, vector<8xi1>, i32) -> vector<8xi32>
  // CHECK-NEXT: "llvm.intr.vp.smin"(%[[ARG0]], %[[ARG1]], %[[ARG2]], %[[ARG3]])
  "llvm.intr.vp.smin" (%A, %B, %mask, %evl) :
         (vector<8xi32>, vector<8xi32>, vector<8xi1>, i32) -> vector<8xi32>
  // CHECK-NEXT: "llvm.intr.vp.umax"(%[[ARG0]], %[[ARG1]], %[[ARG2]], %[[ARG3]])
  "llvm.intr.vp.umax" (%A, %B, %mask, %evl) :
         (vector<8xi32>, vector<8xi32>, vector<8xi1>, i32) -> vector<8xi32>
  // CHECK-NEXT: "llvm.intr.vp.umin"(%[[ARG0]], %[[ARG1]], %[[ARG2]], %[[ARG3]])
  "llvm.intr.vp.umin" (%A, %B, %mask, %evl) :
         (vector<8xi32>, vector<8xi32>, vector<8xi1>, i32) -> vector<8xi32>
  llvm.return
}

llvm.func @op_bundle_target()

// CHECK-LABEL: @test_call_with_empty_opbundle
llvm.func @test_call_with_empty_opbundle() {
  // CHECK: llvm.call @op_bundle_target() : () -> ()
  llvm.call @op_bundle_target() [] : () -> ()
  llvm.return
}

// CHECK-LABEL: @test_call_with_empty_opbundle_operands
llvm.func @test_call_with_empty_opbundle_operands() {
  // CHECK: llvm.call @op_bundle_target() ["tag"()] : () -> ()
  llvm.call @op_bundle_target() ["tag"()] : () -> ()
  llvm.return
}

// CHECK-LABEL: @test_call_with_opbundle
llvm.func @test_call_with_opbundle() {
  %0 = llvm.mlir.constant(0 : i32) : i32
  %1 = llvm.mlir.constant(1 : i32) : i32
  %2 = llvm.mlir.constant(2 : i32) : i32
  // CHECK: llvm.call @op_bundle_target() ["tag1"(%{{.+}}, %{{.+}} : i32, i32), "tag2"(%{{.+}} : i32)] : () -> ()
  llvm.call @op_bundle_target() ["tag1"(%0, %1 : i32, i32), "tag2"(%2 : i32)] : () -> ()
  llvm.return
}

// CHECK-LABEL: @test_invoke_with_empty_opbundle
llvm.func @test_invoke_with_empty_opbundle() attributes { personality = @__gxx_personality_v0 } {
  %0 = llvm.mlir.constant(1 : i32) : i32
  %1 = llvm.mlir.constant(2 : i32) : i32
  %2 = llvm.mlir.constant(3 : i32) : i32
  // CHECK: llvm.invoke @op_bundle_target() to ^{{.+}} unwind ^{{.+}} : () -> ()
  llvm.invoke @op_bundle_target() to ^bb2 unwind ^bb1 [] : () -> ()

^bb1:
  %3 = llvm.landingpad cleanup : !llvm.struct<(ptr, i32)>
  llvm.return

^bb2:
  llvm.return
}

// CHECK-LABEL: @test_invoke_with_empty_opbundle_operands
llvm.func @test_invoke_with_empty_opbundle_operands() attributes { personality = @__gxx_personality_v0 } {
  %0 = llvm.mlir.constant(1 : i32) : i32
  %1 = llvm.mlir.constant(2 : i32) : i32
  %2 = llvm.mlir.constant(3 : i32) : i32
  // CHECK: llvm.invoke @op_bundle_target() to ^{{.+}} unwind ^{{.+}} ["tag"()] : () -> ()
  llvm.invoke @op_bundle_target() to ^bb2 unwind ^bb1 ["tag"()] : () -> ()

^bb1:
  %3 = llvm.landingpad cleanup : !llvm.struct<(ptr, i32)>
  llvm.return

^bb2:
  llvm.return
}

// CHECK-LABEL: @test_invoke_with_opbundle
llvm.func @test_invoke_with_opbundle() attributes { personality = @__gxx_personality_v0 } {
  %0 = llvm.mlir.constant(1 : i32) : i32
  %1 = llvm.mlir.constant(2 : i32) : i32
  %2 = llvm.mlir.constant(3 : i32) : i32
  // CHECK: llvm.invoke @op_bundle_target() to ^{{.+}} unwind ^{{.+}} ["tag1"(%{{.+}}, %{{.+}} : i32, i32), "tag2"(%{{.+}} : i32)] : () -> ()
  llvm.invoke @op_bundle_target() to ^bb2 unwind ^bb1 ["tag1"(%0, %1 : i32, i32), "tag2"(%2 : i32)] : () -> ()

^bb1:
  %3 = llvm.landingpad cleanup : !llvm.struct<(ptr, i32)>
  llvm.return

^bb2:
  llvm.return
}

// CHECK-LABEL: @test_call_intrin_with_opbundle
llvm.func @test_call_intrin_with_opbundle(%arg0 : !llvm.ptr) {
  %0 = llvm.mlir.constant(1 : i1) : i1
  %1 = llvm.mlir.constant(16 : i32) : i32
  // CHECK: llvm.call_intrinsic "llvm.assume"(%{{.+}}) ["align"(%{{.+}}, %{{.+}} : !llvm.ptr, i32)] : (i1) -> ()
  llvm.call_intrinsic "llvm.assume"(%0) ["align"(%arg0, %1 : !llvm.ptr, i32)] : (i1) -> ()
  llvm.return
}

// CHECK-LABEL: @test_assume_intr_no_opbundle
llvm.func @test_assume_intr_no_opbundle(%arg0 : !llvm.ptr) {
  %0 = llvm.mlir.constant(1 : i1) : i1
  // CHECK: llvm.intr.assume %0 : i1
  llvm.intr.assume %0 : i1
  llvm.return
}

// CHECK-LABEL: @test_assume_intr_empty_opbundle
llvm.func @test_assume_intr_empty_opbundle(%arg0 : !llvm.ptr) {
  %0 = llvm.mlir.constant(1 : i1) : i1
  // CHECK: llvm.intr.assume %0 : i1
  llvm.intr.assume %0 [] : i1
  llvm.return
}

// CHECK-LABEL: @test_assume_intr_with_opbundles
llvm.func @test_assume_intr_with_opbundles(%arg0 : !llvm.ptr) {
  %0 = llvm.mlir.constant(1 : i1) : i1
  %1 = llvm.mlir.constant(2 : i32) : i32
  %2 = llvm.mlir.constant(3 : i32) : i32
  %3 = llvm.mlir.constant(4 : i32) : i32
  // CHECK: llvm.intr.assume %0 ["tag1"(%1, %2 : i32, i32), "tag2"(%3 : i32)] : i1
  llvm.intr.assume %0 ["tag1"(%1, %2 : i32, i32), "tag2"(%3 : i32)] : i1
  llvm.return
}

llvm.func @somefunc(i32, !llvm.ptr)

// CHECK-LABEL: llvm.func @test_call_arg_attrs_direct(
// CHECK-SAME:    %[[VAL_0:.*]]: i32,
// CHECK-SAME:    %[[VAL_1:.*]]: !llvm.ptr)
llvm.func @test_call_arg_attrs_direct(%arg0: i32, %arg1: !llvm.ptr) {
  // CHECK: llvm.call @somefunc(%[[VAL_0]], %[[VAL_1]]) : (i32, !llvm.ptr {llvm.byval = i64}) -> ()
  llvm.call @somefunc(%arg0, %arg1) : (i32, !llvm.ptr {llvm.byval = i64}) -> ()
  llvm.return
}

// CHECK-LABEL: llvm.func @test_call_arg_attrs_indirect(
// CHECK-SAME:    %[[VAL_0:.*]]: i16,
// CHECK-SAME:    %[[VAL_1:.*]]: !llvm.ptr
llvm.func @test_call_arg_attrs_indirect(%arg0: i16, %arg1: !llvm.ptr) -> i16 {
  // CHECK: llvm.call tail %[[VAL_1]](%[[VAL_0]]) : !llvm.ptr, (i16 {llvm.noundef, llvm.signext}) -> (i16 {llvm.signext})
  %0 = llvm.call tail %arg1(%arg0) : !llvm.ptr, (i16 {llvm.noundef, llvm.signext}) -> (i16 {llvm.signext})
  llvm.return %0 : i16
}

// CHECK-LABEL:   llvm.func @test_invoke_arg_attrs(
// CHECK-SAME:      %[[VAL_0:.*]]: i16) attributes {personality = @__gxx_personality_v0} {
llvm.func @test_invoke_arg_attrs(%arg0: i16) attributes { personality = @__gxx_personality_v0 } {
  // CHECK:  llvm.invoke @somefunc(%[[VAL_0]]) to ^bb2 unwind ^bb1 : (i16 {llvm.noundef, llvm.signext}) -> ()
  llvm.invoke @somefunc(%arg0) to ^bb2 unwind ^bb1 : (i16 {llvm.noundef, llvm.signext}) -> ()
^bb1:
  %1 = llvm.landingpad cleanup : !llvm.struct<(ptr, i32)>
  llvm.return
^bb2:
  llvm.return
}

// CHECK-LABEL:   llvm.func @test_invoke_arg_attrs_indirect(
// CHECK-SAME:      %[[VAL_0:.*]]: i16,
// CHECK-SAME:      %[[VAL_1:.*]]: !llvm.ptr) -> i16 attributes {personality = @__gxx_personality_v0} {
llvm.func @test_invoke_arg_attrs_indirect(%arg0: i16, %arg1: !llvm.ptr) -> i16 attributes { personality = @__gxx_personality_v0 } {
  // CHECK: llvm.invoke %[[VAL_1]](%[[VAL_0]]) to ^bb2 unwind ^bb1 : !llvm.ptr, (i16 {llvm.noundef, llvm.signext}) -> (i16 {llvm.signext})
  %0 = llvm.invoke %arg1(%arg0) to ^bb2 unwind ^bb1 : !llvm.ptr, (i16 {llvm.noundef, llvm.signext}) -> (i16 {llvm.signext})
^bb1:
  %1 = llvm.landingpad cleanup : !llvm.struct<(ptr, i32)>
  llvm.return %0 : i16
^bb2:
  llvm.return %0 : i16
}

// CHECK-LABEL: intrinsic_call_arg_attrs
llvm.func @intrinsic_call_arg_attrs(%arg0: i32) -> i32 {
  // CHECK: %{{.*}} = llvm.call_intrinsic "llvm.riscv.sha256sig0"({{.*}}) : (i32 {llvm.signext}) -> i32
  %0 = llvm.call_intrinsic "llvm.riscv.sha256sig0"(%arg0) : (i32 {llvm.signext}) -> (i32)
  llvm.return %0 : i32
}

// CHECK-LABEL: intrinsic_call_arg_attrs_bundles
llvm.func @intrinsic_call_arg_attrs_bundles(%arg0: i32) -> i32 {
  // CHECK: %{{.*}} = llvm.call_intrinsic "llvm.riscv.sha256sig0"({{.*}}) ["adazdazd"()] {constant} : (i32 {llvm.signext}) -> i32
  %0 = llvm.call_intrinsic "llvm.riscv.sha256sig0"(%arg0) ["adazdazd"()] {constant} : (i32 {llvm.signext}) -> (i32)
  llvm.return %0 : i32
}

llvm.mlir.global private @blockaddr_global() {addr_space = 0 : i32, dso_local} : !llvm.ptr {
  %0 = llvm.blockaddress <function = @blockaddr_fn, tag = <id = 0>> : !llvm.ptr
  llvm.return %0 : !llvm.ptr
}

// CHECK: llvm.mlir.global private @blockaddr_global() {{.*}}
// CHECK-NEXT:   %{{.*}} = llvm.blockaddress <function = @blockaddr_fn, tag = <id = 0>> : !llvm.ptr
// CHECK-NEXT:   llvm.return %{{.*}} : !llvm.ptr

llvm.func @blockaddr_fn() {
  llvm.br ^bb1
^bb1:
  llvm.blocktag <id = 0>
  llvm.return
}

// CHECK-LABEL: llvm.func @blockaddr_fn
// CHECK-NEXT:  llvm.br ^bb1
// CHECK-NEXT:^bb1:
// CHECK-NEXT:  llvm.blocktag <id = 0>

llvm.func @callintrin_voidret(%arg0: vector<8xi8>, %arg1: vector<8xi8>, %arg2: vector<8xi8>, %arg3: !llvm.ptr) {
  llvm.call_intrinsic "llvm.aarch64.neon.st3.v8i8.p0"(%arg0, %arg1, %arg2, %arg3) : (vector<8xi8>, vector<8xi8>, vector<8xi8>, !llvm.ptr) -> ()
  llvm.return
}
llvm.func @llvm.aarch64.neon.st3.v8i8.p0(vector<8xi8>, vector<8xi8>, vector<8xi8>, !llvm.ptr)

// CHECK-LABEL: llvm.func @callintrin_voidret
// CHECK-NEXT: llvm.call_intrinsic "llvm.aarch64.neon.st3.v8i8.p0"(%arg0, %arg1, %arg2, %arg3) : (vector<8xi8>, vector<8xi8>, vector<8xi8>, !llvm.ptr) -> ()

llvm.mlir.global internal thread_local unnamed_addr @myglobal(-1 : i32) {addr_space = 0 : i32, alignment = 4 : i64, dso_local} : i32
// CHECK: llvm.mlir.global internal thread_local unnamed_addr @myglobal(-1 : i32) {addr_space = 0 : i32, alignment = 4 : i64, dso_local} : i32
