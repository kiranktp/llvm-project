// RUN: mlir-opt --verify-each --split-input-file -pass-pipeline="builtin.module(func.func(tosa-to-linalg-named))" %s -verify-diagnostics -o -| FileCheck %s
// RUN: mlir-opt --verify-each --split-input-file -pass-pipeline="builtin.module(func.func(tosa-to-linalg-named{prefer-conv2d-kernel-layout-hwcf=true}))" %s -verify-diagnostics -o -| FileCheck --check-prefix="HWCF" %s
// RUN: mlir-opt --verify-each --split-input-file -pass-pipeline="builtin.module(func.func(tosa-to-linalg-named,cse))" %s -verify-diagnostics -o -| FileCheck --check-prefix="CHECK-CSE" %s

// CHECK-LABEL: @matmul
func.func @matmul(%arg0: tensor<1x5x3xf32>, %arg1: tensor<1x3x6xf32>) -> (tensor<1x5x6xf32>) {
  // CHECK: [[C0:%.+]] = arith.constant 0
  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[FILLED:%.+]] = linalg.fill ins([[C0]] : f32) outs([[INIT]] : tensor<1x5x6xf32>) -> tensor<1x5x6xf32>
  // CHECK: linalg.batch_matmul ins(%arg0, %arg1 : tensor<1x5x3xf32>, tensor<1x3x6xf32>) outs([[FILLED]] : tensor<1x5x6xf32>) -> tensor<1x5x6xf32>
  %a_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %b_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.matmul %arg0, %arg1, %a_zp, %b_zp : (tensor<1x5x3xf32>, tensor<1x3x6xf32>, tensor<1xf32>, tensor<1xf32>)  -> tensor<1x5x6xf32>
  return %0 : tensor<1x5x6xf32>
}

// -----


// CHECK-LABEL: @matmul_quantized
func.func @matmul_quantized(%arg0: tensor<1x5x3xi8>, %arg1: tensor<1x3x6xi8>) -> (tensor<1x5x6xi32>) {
  // CHECK: [[C0:%.+]] = arith.constant 0
  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[FILLED:%.+]] = linalg.fill ins([[C0]] : i32) outs([[INIT]] : tensor<1x5x6xi32>) -> tensor<1x5x6xi32>
  // CHECK: [[ONE:%.+]] = arith.constant 1
  // CHECK: [[TWO:%.+]] = arith.constant 2
  // CHECK: linalg.quantized_batch_matmul ins(%arg0, %arg1, [[ONE]], [[TWO]] : tensor<1x5x3xi8>, tensor<1x3x6xi8>, i32, i32) outs([[FILLED]] : tensor<1x5x6xi32>) -> tensor<1x5x6xi32>
  %a_zp = "tosa.const"() <{values = dense<1> : tensor<1xi8>}> : () -> tensor<1xi8>
  %b_zp = "tosa.const"() <{values = dense<2> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.matmul %arg0, %arg1, %a_zp, %b_zp : (tensor<1x5x3xi8>, tensor<1x3x6xi8>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x5x6xi32>
  return %0 : tensor<1x5x6xi32>
}

// -----

// CHECK-LABEL: @matmul_dyn_batch
func.func @matmul_dyn_batch(%arg0: tensor<?x5x3xf32>, %arg1: tensor<?x3x6xf32>) -> (tensor<?x5x6xf32>) {
  // CHECK: %[[C0:.+]] = arith.constant 0
  // CHECK: %[[DIM:.+]] = tensor.dim %arg0, %[[C0]]
  // CHECK: %[[C0_0:.+]] = arith.constant 0
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[DIM]])
  // CHECK: %[[FILLED:.+]] = linalg.fill ins(%[[C0_0]] : f32) outs(%[[INIT]] : tensor<?x5x6xf32>) -> tensor<?x5x6xf32>
  // CHECK: linalg.batch_matmul ins(%arg0, %arg1 : tensor<?x5x3xf32>, tensor<?x3x6xf32>) outs(%[[FILLED]] : tensor<?x5x6xf32>) -> tensor<?x5x6xf32>
  %a_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %b_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.matmul %arg0, %arg1, %a_zp, %b_zp : (tensor<?x5x3xf32>, tensor<?x3x6xf32>, tensor<1xf32>, tensor<1xf32>)  -> tensor<?x5x6xf32>
  return %0 : tensor<?x5x6xf32>
}

// -----

// CHECK-LABEL: @matmul_dyn_independent_dim
func.func @matmul_dyn_independent_dim(%arg0: tensor<1x5x3xf32>, %arg1: tensor<1x3x?xf32>) -> (tensor<1x5x?xf32>) {
  // CHECK: %[[C2:.+]] = arith.constant 2
  // CHECK: %[[DIM:.+]] = tensor.dim %arg1, %[[C2]]
  // CHECK: %[[C0:.+]] = arith.constant 0
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[DIM]])
  // CHECK: %[[FILLED:.+]] = linalg.fill ins(%[[C0]] : f32) outs(%[[INIT]] : tensor<1x5x?xf32>) -> tensor<1x5x?xf32>
  // CHECK: linalg.batch_matmul ins(%arg0, %arg1 : tensor<1x5x3xf32>, tensor<1x3x?xf32>) outs(%[[FILLED]] : tensor<1x5x?xf32>) -> tensor<1x5x?xf32>
  %a_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %b_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.matmul %arg0, %arg1, %a_zp, %b_zp : (tensor<1x5x3xf32>, tensor<1x3x?xf32>, tensor<1xf32>, tensor<1xf32>)  -> tensor<1x5x?xf32>
  return %0 : tensor<1x5x?xf32>
}

// -----

// CHECK-LABEL: @matmul_dyn_independent_dim
func.func @matmul_dyn_independent_dim(%arg0: tensor<1x5x?xf32>, %arg1: tensor<1x?x6xf32>) -> (tensor<1x5x6xf32>) {
  // CHECK: %[[C0:.+]] = arith.constant 0
  // CHECK: %[[INIT:.+]] = tensor.empty()
  // CHECK: %[[FILLED:.+]] = linalg.fill ins(%[[C0]] : f32) outs(%[[INIT]] : tensor<1x5x6xf32>) -> tensor<1x5x6xf32>
  // CHECK: linalg.batch_matmul ins(%arg0, %arg1 : tensor<1x5x?xf32>, tensor<1x?x6xf32>) outs(%[[FILLED]] : tensor<1x5x6xf32>) -> tensor<1x5x6xf32>
  %a_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %b_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.matmul %arg0, %arg1, %a_zp, %b_zp : (tensor<1x5x?xf32>, tensor<1x?x6xf32>, tensor<1xf32>, tensor<1xf32>)  -> tensor<1x5x6xf32>
  return %0 : tensor<1x5x6xf32>
}

// -----

// CHECK-LABEL: @matmul_dyn_output
func.func @matmul_dyn_output(%arg0: tensor<1x1x8xf32>, %arg1: tensor<1x8x1xf32>) -> tensor<?x1x1xf32> {
  // CHECK: %[[C0:.+]] = arith.constant 0 : index
  // CHECK: %[[DIM0:.+]] = tensor.dim %arg0, %[[C0]] : tensor<1x1x8xf32>
  // CHECK: %[[CST:.+]] = arith.constant 0.000000e+00 : f32
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[DIM0]]) : tensor<?x1x1xf32>
  // CHECK: %[[FILLED:.+]] = linalg.fill ins(%[[CST]] : f32) outs(%[[INIT]] : tensor<?x1x1xf32>) -> tensor<?x1x1xf32>
  // CHECK: linalg.batch_matmul ins(%arg0, %arg1 : tensor<1x1x8xf32>, tensor<1x8x1xf32>) outs(%[[FILLED]] : tensor<?x1x1xf32>) -> tensor<?x1x1xf32>
  %a_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %b_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.matmul %arg0, %arg1, %a_zp, %b_zp : (tensor<1x1x8xf32>, tensor<1x8x1xf32>, tensor<1xf32>, tensor<1xf32>)  -> tensor<?x1x1xf32>
  return %0 : tensor<?x1x1xf32>
}

// -----

// CHECK-LABEL: @max_pool
func.func @max_pool(%arg0: tensor<1x6x34x62xf32>) -> () {
  // CHECK-DAG: [[CONST:%.+]] = arith.constant -3.40282347E+38
  // CHECK-DAG: [[INIT:%.+]] = tensor.empty()
  // CHECK-DAG: [[FILL:%.+]] = linalg.fill ins([[CONST]]{{.*}}outs([[INIT]]
  // CHECK-DAG: [[KERNEL:%.+]] = tensor.empty()
  // CHECK: linalg.pooling_nhwc_max {dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>} ins(%arg0, [[KERNEL]] : tensor<1x6x34x62xf32>, tensor<3x3xf32>) outs([[FILL]] : tensor<1x4x32x62xf32>)
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xf32>) -> tensor<1x4x32x62xf32>
  return
}

// CHECK-LABEL: @max_pool_padded
func.func @max_pool_padded(%arg0: tensor<1x6x34x62xf32>) -> () {
  // CHECK-DAG: [[CONST:%.+]] = arith.constant -3.40282347E+38 : f32
  // CHECK-DAG: [[PAD:%.+]] = tensor.pad %arg0 low[0, 0, 0, 0] high[0, 0, 1, 0]
  // CHECK-DAG:   tensor.yield [[CONST]]
  // CHECK-DAG: [[INITVAL:%.+]] = arith.constant -3.40282347E+38 : f32
  // CHECK-DAG: [[INIT:%.+]] = tensor.empty()
  // CHECK-DAG: [[FILL:%.+]] = linalg.fill ins([[INITVAL]]{{.*}}outs([[INIT]]
  // CHECK-DAG: [[KERNEL:%.+]] = tensor.empty()
  // CHECK: linalg.pooling_nhwc_max {dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>} ins([[PAD]], [[KERNEL]] : tensor<1x6x35x62xf32>, tensor<3x3xf32>) outs([[FILL]] : tensor<1x4x33x62xf32>)
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 1>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xf32>) -> tensor<1x4x33x62xf32>
  return
}

// CHECK-LABEL: @max_pool_dyn
func.func @max_pool_dyn(%arg0: tensor<?x6x34x62xf32>) -> () {
  // CHECK: %[[C0:.+]] = arith.constant 0
  // CHECK: %[[BATCH:.+]] = tensor.dim %arg0, %[[C0]]
  // CHECK: %[[CONST:.+]] = arith.constant -3.40282347E+38
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[BATCH]])
  // CHECK: %[[FILL:.+]] = linalg.fill ins(%[[CONST]]{{.*}}outs(%[[INIT]]
  // CHECK: %[[KERNEL:.+]] = tensor.empty()
  // CHECK: linalg.pooling_nhwc_max {dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>} ins(%arg0, %[[KERNEL]] : tensor<?x6x34x62xf32>, tensor<3x3xf32>) outs(%[[FILL]] : tensor<?x4x32x62xf32>)
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<?x6x34x62xf32>) -> tensor<?x4x32x62xf32>
  return
}

// CHECK-LABEL: @max_pool_i8
func.func @max_pool_i8(%arg0: tensor<1x6x34x62xi8>) -> () {
  // CHECK: arith.constant -128
  // CHECK: linalg.pooling_nhwc_max
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xi8>) -> tensor<1x4x32x62xi8>
  return
}

// CHECK-LABEL: @max_pool_ui8
func.func @max_pool_ui8(%arg0: tensor<1x6x34x62xui8>) -> tensor<1x4x32x62xui8> {
  // CHECK: builtin.unrealized_conversion_cast {{.*}} : tensor<1x6x34x62xui8> to tensor<1x6x34x62xi8>
  // CHECK: arith.constant 0
  // CHECK: linalg.pooling_nhwc_max_unsigned
  // CHECK-SAME: ins({{.*}} : tensor<1x6x34x62xi8>, tensor<3x3xi8>)
  // CHECK-SAME: outs({{.*}} : tensor<1x4x32x62xi8>)
  // CHECK-SAME: -> tensor<1x4x32x62xi8>
  // CHECK: builtin.unrealized_conversion_cast {{.*}} : tensor<1x4x32x62xi8> to tensor<1x4x32x62xui8>
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xui8>) -> tensor<1x4x32x62xui8>
  return %0 : tensor<1x4x32x62xui8>
}

// CHECK-LABEL: @max_pool_i16
func.func @max_pool_i16(%arg0: tensor<1x6x34x62xi16>) -> () {
  // CHECK: arith.constant -32768
  // CHECK: linalg.pooling_nhwc_max
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xi16>) -> tensor<1x4x32x62xi16>
  return
}

// CHECK-LABEL: @max_pool_i32
func.func @max_pool_i32(%arg0: tensor<1x6x34x62xi32>) -> () {
  // CHECK: arith.constant -2147483648
  // CHECK: linalg.pooling_nhwc_max
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xi32>) -> tensor<1x4x32x62xi32>
  return
}

// CHECK-CSE-LABEL: @max_pool_all_dynamic
func.func @max_pool_all_dynamic(%arg0: tensor<?x?x?x?xf32>) -> tensor<?x?x?x?xf32> {
  // Batch size
  // CHECK-CSE: %[[C0:.+]] = arith.constant 0 : index
  // CHECK-CSE: %[[BATCH:.+]] = tensor.dim %arg0, %[[C0]] : tensor<?x?x?x?xf32>

  // Compute output height
  // CHECK-CSE: %[[C1:.+]] = arith.constant 1 : index
  // CHECK-CSE: %[[IH:.+]] = tensor.dim %arg0, %[[C1]] : tensor<?x?x?x?xf32>
  // CHECK-CSE: %[[C2:.+]] = arith.constant 2 : index
  // CHECK-CSE: %[[PADDED_BEFORE:.+]] = arith.addi %[[IH]], %[[C0]] : index
  // CHECK-CSE: %[[PADDED_AFTER:.+]] = arith.addi %[[PADDED_BEFORE]], %[[C0]] : index
  // CHECK-CSE: %[[SUB_ONE:.+]] = arith.subi %[[C2]], %[[C1]] : index
  // CHECK-CSE: %[[DILATED:.+]] = arith.muli %[[C1]], %[[SUB_ONE]] : index
  // CHECK-CSE: %[[ADD_ONE:.+]] = arith.addi %[[DILATED]], %[[C1]] : index
  // CHECK-CSE: %[[SUBTRACT:.+]] = arith.subi %[[PADDED_AFTER]], %[[ADD_ONE]] : index
  // CHECK-CSE: %[[DIVIDE:.+]] = arith.divui %[[SUBTRACT]], %[[C1]] : index
  // CHECK-CSE: %[[HEIGHT:.+]] = arith.addi %[[DIVIDE]], %[[C1]] : index

  // Compute output width
  // CHECK-CSE: %[[IW:.+]] = tensor.dim %arg0, %[[C2]] : tensor<?x?x?x?xf32>
  // CHECK-CSE: %[[C5:.+]] = arith.constant 5 : index
  // CHECK-CSE: %[[PADDED_BEFORE:.+]] = arith.addi %[[IW]], %[[C2]] : index
  // CHECK-CSE: %[[PADDED_AFTER:.+]] = arith.addi %[[PADDED_BEFORE]], %[[C2]] : index
  // CHECK-CSE: %[[SUB_ONE:.+]] = arith.subi %[[C5]], %[[C1]] : index
  // CHECK-CSE: %[[DILATED:.+]] = arith.muli %[[C1]], %[[SUB_ONE]] : index
  // CHECK-CSE: %[[ADD_ONE:.+]] = arith.addi %[[DILATED]], %[[C1]] : index
  // CHECK-CSE: %[[SUBTRACT:.+]] = arith.subi %[[PADDED_AFTER]], %[[ADD_ONE]] : index
  // CHECK-CSE: %[[DIVIDE:.+]] = arith.divui %[[SUBTRACT]], %[[C1]] : index
  // CHECK-CSE: %[[WIDTH:.+]] = arith.addi %14, %[[C1]] : index

  // Channel size
  // CHECK-CSE: %[[C3:.+]] = arith.constant 3 : index
  // CHECK-CSE: %[[CHANNEL:.+]] = tensor.dim %arg0, %[[C3]] : tensor<?x?x?x?xf32>

  // Pad the input
  // CHECK-CSE: %[[FLOAT_MIN:.+]] = arith.constant -3.40282347E+38 : f32
  // CHECK-CSE: %[[PADDED:.+]] = tensor.pad %arg0 low[0, 0, 2, 0] high[0, 0, 2, 0] {
  // CHECK-CSE:   tensor.yield %[[FLOAT_MIN]] : f32

  // Allocate the output and fill with minimum value
  // CHECK-CSE: %[[INIT:.+]] = tensor.empty(%[[BATCH]], %[[HEIGHT]], %[[WIDTH]], %[[CHANNEL]]) : tensor<?x?x?x?xf32>
  // CHECK-CSE: %[[FILL:.+]] = linalg.fill ins(%[[FLOAT_MIN]] : f32) outs(%[[INIT]] : tensor<?x?x?x?xf32>) -> tensor<?x?x?x?xf32>
  // CHECK-CSE: %[[FAKE_WINDOW:.+]] = tensor.empty() : tensor<2x5xf32>

  // Compute max pool
  // CHECK-CSE: %[[OUT:.+]] = linalg.pooling_nhwc_max {dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>} ins(%[[PADDED]], %[[FAKE_WINDOW]] : tensor<?x?x?x?xf32>, tensor<2x5xf32>) outs(%[[FILL]] : tensor<?x?x?x?xf32>) -> tensor<?x?x?x?xf32>
  // CHECK-CSE: return %[[OUT]]

  %0 = tosa.max_pool2d %arg0 {kernel = array<i64: 2, 5>, pad = array<i64: 0, 0, 2, 2>, stride = array<i64: 1, 1>} : (tensor<?x?x?x?xf32>) -> tensor<?x?x?x?xf32>
  return %0 : tensor<?x?x?x?xf32>
}

// -----

// CHECK-LABEL: @avg_pool_f32
func.func @avg_pool_f32(%arg0: tensor<1x6x34x62xf32>) -> (tensor<1x5x33x62xf32>) {
  // Apply padding to the input:
  // CHECK: %[[F0:.+]] = arith.constant 0.000000e+00 : f32
  // CHECK: %[[PAD:.+]] = tensor.pad %arg0 low[0, 1, 1, 0] high[0, 1, 1, 0]
  // CHECK:   tensor.yield %[[F0]] : f32

  // Fill the pooling target:
  // CHECK: %[[F0:.+]] = arith.constant 0.000000e+00 : f32
  // CHECK: %[[EMPTY:.+]] = tensor.empty() : tensor<1x5x33x62xf32>
  // CHECK: %[[FILL:.+]] = linalg.fill ins(%[[F0]] : f32) outs(%[[EMPTY]] : tensor<1x5x33x62xf32>)

  // Compute the sum padding:
  // CHECK: %[[KERNEL:.+]] = tensor.empty() : tensor<4x4xf32>
  // CHECK: %[[POOL:.+]] = linalg.pooling_nhwc_sum
  // CHECK-SAME: dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>}
  // CHECK-SAME: ins(%[[PAD]], %[[KERNEL]] : tensor<1x8x36x62xf32>, tensor<4x4xf32>)
  // CHECK-SAME: outs(%[[FILL]] : tensor<1x5x33x62xf32>)

  // Compute dimension based constants:
  // CHECK: %[[I1:.+]] = arith.constant 1 : index
  // CHECK: %[[DIM1:.+]] = tensor.dim %[[POOL]], %[[I1]]
  // CHECK: %[[I2:.+]] = arith.constant 2 : index
  // CHECK: %[[DIM2:.+]] = tensor.dim %[[POOL]], %[[I2]]
  // CHECK: %[[ONE:.+]] = arith.constant 1 : index
  // CHECK: %[[HEIGHT:.+]] = arith.subi %[[DIM1]], %[[ONE]] : index
  // CHECK: %[[WIDTH:.+]] = arith.subi %[[DIM2]], %[[ONE]] : index

  // Divide the sum pooling by the number of summed values.
  // CHECK: %[[EMPTY:.+]] = tensor.empty() : tensor<1x5x33x62xf32>
  // CHECK: %[[GENERIC:.+]] = linalg.generic
  // CHECK-SAME: indexing_maps = [#map, #map], iterator_types = ["parallel", "parallel", "parallel", "parallel"]}
  // CHECK-SAME: ins(%[[POOL]] : tensor<1x5x33x62xf32>)
  // CHECK-SAME: outs(%[[EMPTY]] : tensor<1x5x33x62xf32>)
  // CHECK: ^bb0(%[[IN:.+]]: f32, %{{.+}}: f32)
  // CHECK:   %[[ZERO:.+]] = arith.constant 0

  // Compute how much of the height does not include padding:
  // CHECK:   %[[STRIDE:.+]] = arith.constant 1
  // CHECK:   %[[KSIZE:.+]] = arith.constant 4
  // CHECK:   %[[START:.+]] = linalg.index 1
  // CHECK:   %[[END:.+]] = arith.subi %[[HEIGHT]], %[[START]]
  // CHECK:   %[[SRC_START:.+]] = arith.muli %[[START]], %[[STRIDE]]
  // CHECK:   %[[SRC_END:.+]] = arith.muli %[[END]], %[[STRIDE]]
  // CHECK:   %[[PAD_START:.+]] = arith.constant 1
  // CHECK:   %[[START_SUB:.+]] = arith.subi %[[SRC_START]], %[[PAD_START]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[START_SUB]], %[[ZERO]]
  // CHECK:   %[[START_OFFSET:.+]] = arith.addi %[[KSIZE]], %[[OFFSET]]
  // CHECK:   %[[PAD_END:.+]] = arith.constant 1
  // CHECK:   %[[END_SUB:.+]] = arith.subi %[[SRC_END]], %[[PAD_END]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[END_SUB]], %[[ZERO]]
  // CHECK:   %[[END_OFFSET:.+]] = arith.addi %[[START_OFFSET]], %[[OFFSET]]
  // CHECK:   %[[KHEIGHT:.+]] = arith.maxsi %[[ONE]], %[[END_OFFSET]]

  // Compute how much of the width does not include padding:
  // CHECK:   %[[STRIDE:.+]] = arith.constant 1
  // CHECK:   %[[KSIZE:.+]] = arith.constant 4
  // CHECK:   %[[START:.+]] = linalg.index 2
  // CHECK:   %[[END:.+]] = arith.subi %[[WIDTH]], %[[START]]
  // CHECK:   %[[SRC_START:.+]] = arith.muli %[[START]], %[[STRIDE]]
  // CHECK:   %[[SRC_END:.+]] = arith.muli %[[END]], %[[STRIDE]]
  // CHECK:   %[[PAD_START:.+]] = arith.constant 1
  // CHECK:   %[[START_SUB:.+]] = arith.subi %[[SRC_START]], %[[PAD_START]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[START_SUB]], %[[ZERO]]
  // CHECK:   %[[START_OFFSET:.+]] = arith.addi %[[KSIZE]], %[[OFFSET]]
  // CHECK:   %[[PAD_END:.+]] = arith.constant 1
  // CHECK:   %[[END_SUB:.+]] = arith.subi %[[SRC_END]], %[[PAD_END]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[END_SUB]], %[[ZERO]]
  // CHECK:   %[[END_OFFSET:.+]] = arith.addi %[[START_OFFSET]], %[[OFFSET]]
  // CHECK:   %[[KWIDTH:.+]] = arith.maxsi %[[ONE]], %[[END_OFFSET]]

  // Divide the summed value by the number of values summed.
  // CHECK:   %[[COUNT:.+]] = arith.muli %[[KHEIGHT]], %[[KWIDTH]]
  // CHECK:   %[[CAST:.+]] = arith.index_cast %[[COUNT]]
  // CHECK:   %[[FLT:.+]] = arith.sitofp %[[CAST]]
  // CHECK:   %[[DIV:.+]] = arith.divf %[[IN]], %[[FLT]]
  // CHECK:   linalg.yield %[[DIV]]
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %output_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.avg_pool2d %arg0, %input_zp, %output_zp {acc_type = f32, pad = array<i64: 1, 1, 1, 1>, kernel = array<i64: 4, 4>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x5x33x62xf32>
  return %0 : tensor<1x5x33x62xf32>
}

// -----

// CHECK-LABEL: @avg_pool_f16_f32acc
// CHECK-SAME: (%[[ARG0:[0-9a-zA-Z_]*]]:
func.func @avg_pool_f16_f32acc(%arg0: tensor<1x6x34x62xf16>) -> (tensor<1x5x33x62xf16>) {
  // Apply padding to the input:
  // CHECK: %[[F0:.+]] = arith.constant 0.000000e+00 : f16
  // CHECK: %[[PAD:.+]] = tensor.pad %arg0 low[0, 1, 1, 0] high[0, 1, 1, 0]
  // CHECK:   tensor.yield %[[F0]] : f16

  // Fill the pooling target:
  // CHECK: %[[F0:.+]] = arith.constant 0.000000e+00 : f32
  // CHECK: %[[EMPTY:.+]] = tensor.empty() : tensor<1x5x33x62xf32>
  // CHECK: %[[FILL:.+]] = linalg.fill ins(%[[F0]] : f32) outs(%[[EMPTY]] : tensor<1x5x33x62xf32>)

  // Compute the sum padding:
  // CHECK: %[[KERNEL:.+]] = tensor.empty() : tensor<4x4xf32>
  // CHECK: %[[POOL:.+]] = linalg.pooling_nhwc_sum
  // CHECK-SAME: dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>}
  // CHECK-SAME: ins(%[[PAD]], %[[KERNEL]] : tensor<1x8x36x62xf16>, tensor<4x4xf32>)
  // CHECK-SAME: outs(%[[FILL]] : tensor<1x5x33x62xf32>)

  // Compute dimension based constants:
  // CHECK: %[[I1:.+]] = arith.constant 1 : index
  // CHECK: %[[DIM1:.+]] = tensor.dim %[[POOL]], %[[I1]]
  // CHECK: %[[I2:.+]] = arith.constant 2 : index
  // CHECK: %[[DIM2:.+]] = tensor.dim %[[POOL]], %[[I2]]
  // CHECK: %[[ONE:.+]] = arith.constant 1 : index
  // CHECK: %[[HEIGHT:.+]] = arith.subi %[[DIM1]], %[[ONE]] : index
  // CHECK: %[[WIDTH:.+]] = arith.subi %[[DIM2]], %[[ONE]] : index

  // Divide the sum pooling by the number of summed values.
  // CHECK: %[[EMPTY:.+]] = tensor.empty() : tensor<1x5x33x62xf16>
  // CHECK: %[[GENERIC:.+]] = linalg.generic
  // CHECK-SAME: indexing_maps = [#map, #map], iterator_types = ["parallel", "parallel", "parallel", "parallel"]}
  // CHECK-SAME: ins(%[[POOL]] : tensor<1x5x33x62xf32>)
  // CHECK-SAME: outs(%[[EMPTY]] : tensor<1x5x33x62xf16>)
  // CHECK: ^bb0(%[[IN:.+]]: f32, %{{.+}}: f16)
  // CHECK:   %[[ZERO:.+]] = arith.constant 0

  // Compute how much of the height does not include padding:
  // CHECK:   %[[STRIDE:.+]] = arith.constant 1
  // CHECK:   %[[KSIZE:.+]] = arith.constant 4
  // CHECK:   %[[START:.+]] = linalg.index 1
  // CHECK:   %[[END:.+]] = arith.subi %[[HEIGHT]], %[[START]]
  // CHECK:   %[[SRC_START:.+]] = arith.muli %[[START]], %[[STRIDE]]
  // CHECK:   %[[SRC_END:.+]] = arith.muli %[[END]], %[[STRIDE]]
  // CHECK:   %[[PAD_START:.+]] = arith.constant 1
  // CHECK:   %[[START_SUB:.+]] = arith.subi %[[SRC_START]], %[[PAD_START]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[START_SUB]], %[[ZERO]]
  // CHECK:   %[[START_OFFSET:.+]] = arith.addi %[[KSIZE]], %[[OFFSET]]
  // CHECK:   %[[PAD_END:.+]] = arith.constant 1
  // CHECK:   %[[END_SUB:.+]] = arith.subi %[[SRC_END]], %[[PAD_END]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[END_SUB]], %[[ZERO]]
  // CHECK:   %[[END_OFFSET:.+]] = arith.addi %[[START_OFFSET]], %[[OFFSET]]
  // CHECK:   %[[KHEIGHT:.+]] = arith.maxsi %[[ONE]], %[[END_OFFSET]]

  // Compute how much of the width does not include padding:
  // CHECK:   %[[STRIDE:.+]] = arith.constant 1
  // CHECK:   %[[KSIZE:.+]] = arith.constant 4
  // CHECK:   %[[START:.+]] = linalg.index 2
  // CHECK:   %[[END:.+]] = arith.subi %[[WIDTH]], %[[START]]
  // CHECK:   %[[SRC_START:.+]] = arith.muli %[[START]], %[[STRIDE]]
  // CHECK:   %[[SRC_END:.+]] = arith.muli %[[END]], %[[STRIDE]]
  // CHECK:   %[[PAD_START:.+]] = arith.constant 1
  // CHECK:   %[[START_SUB:.+]] = arith.subi %[[SRC_START]], %[[PAD_START]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[START_SUB]], %[[ZERO]]
  // CHECK:   %[[START_OFFSET:.+]] = arith.addi %[[KSIZE]], %[[OFFSET]]
  // CHECK:   %[[PAD_END:.+]] = arith.constant 1
  // CHECK:   %[[END_SUB:.+]] = arith.subi %[[SRC_END]], %[[PAD_END]]
  // CHECK:   %[[OFFSET:.+]] = arith.minsi %[[END_SUB]], %[[ZERO]]
  // CHECK:   %[[END_OFFSET:.+]] = arith.addi %[[START_OFFSET]], %[[OFFSET]]
  // CHECK:   %[[KWIDTH:.+]] = arith.maxsi %[[ONE]], %[[END_OFFSET]]

  // Divide the summed value by the number of values summed.
  // CHECK:   %[[COUNT:.+]] = arith.muli %[[KHEIGHT]], %[[KWIDTH]]
  // CHECK:   %[[CAST:.+]] = arith.index_cast %[[COUNT]]
  // CHECK:   %[[FLT:.+]] = arith.sitofp %[[CAST]]
  // CHECK:   %[[DIV:.+]] = arith.divf %[[IN]], %[[FLT]]
  // CHECK:   %[[TRUNC:.+]] = arith.truncf %[[DIV]]
  // CHECK:   linalg.yield %[[TRUNC]]
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  %output_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  %0 = tosa.avg_pool2d %arg0, %input_zp, %output_zp {acc_type = f32, pad = array<i64: 1, 1, 1, 1>, kernel = array<i64: 4, 4>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xf16>, tensor<1xf16>, tensor<1xf16>) -> tensor<1x5x33x62xf16>
  return %0 : tensor<1x5x33x62xf16>
}

// -----

// CHECK-LABEL: @avg_pool_i8
func.func @avg_pool_i8(%arg0: tensor<1x6x34x62xi8>) -> (tensor<1x5x33x62xi8>) {
  // CHECK: %[[GENERIC:.+]] = linalg.generic
  // CHECK-SAME: indexing_maps = [#map, #map], iterator_types = ["parallel", "parallel", "parallel", "parallel"]}
  // CHECK-SAME: ins(%[[POOL:.+]] : tensor<1x5x33x62xi32>)
  // CHECK-SAME: outs(%[[EMPTY:.+]] : tensor<1x5x33x62xi8>)
  // CHECK: ^bb0(%[[IN:.+]]: i32, %{{.+}}: i8)

  // Only different behavior is how the division is performed.
  // First we compute the mul and shift values for average pool:
  // CHECK: %[[COUNT:.+]] = arith.muli %{{[0-9]+}}, %{{[0-9]+}}
  // CHECK: %[[ICAST:.+]] = arith.index_cast %[[COUNT]]
  // CHECK: %[[C1:.+]] = arith.constant 1
  // CHECK: %[[C32:.+]] = arith.constant 32
  // CHECK: %[[ISUB:.+]] = arith.subi %[[ICAST]], %[[C1]]
  // CHECK: %[[CTLZ:.+]] = math.ctlz %[[ISUB]]
  // CHECK: %[[SUB:.+]] = arith.subi %[[C32]], %[[CTLZ]]
  // CHECK: %[[EXT:.+]] = arith.extui %[[SUB]]
  // CHECK: %[[CBIG:.+]] = arith.constant 1073741825
  // CHECK: %[[SHL:.+]] = arith.shli %[[CBIG]], %[[EXT]]
  // CHECK: %[[IEXT:.+]] = arith.extui %[[ICAST]]
  // CHECK: %[[DIV:.+]] = arith.divui %[[SHL]], %[[IEXT]]
  // CHECK: %[[TRUNC_MUL:.+]] = arith.trunci %[[DIV]]
  // CHECK: %[[TRUNC_SHIFT:.+]] = arith.trunci %[[SUB]]
  // CHECK: %[[C30:.+]] = arith.constant 30
  // CHECK: %[[SHIFT:.+]] = arith.addi %[[TRUNC_SHIFT]], %[[C30]] : i8
  // CHECK: %[[SCALED:.+]] = tosa.apply_scale %[[IN]], %[[TRUNC_MUL]], %[[SHIFT]] {rounding_mode = "SINGLE_ROUND"}

  // Perform the normalization.
  // CHECK: %[[CMIN:.+]] = arith.constant -128
  // CHECK: %[[CMAX:.+]] = arith.constant 127
  // CHECK: %[[LOW:.+]] = arith.maxsi %[[CMIN]], %[[SCALED]]
  // CHECK: %[[CLAMP:.+]] = arith.minsi %[[CMAX]], %[[LOW]]
  // CHECK: %[[TRUNC:.+]] = arith.trunci %[[CLAMP]]
  // CHECK: linalg.yield %[[TRUNC]]
  %input_zp = "tosa.const"() <{values = dense<0> : tensor<1xi8>}> : () -> tensor<1xi8>
  %output_zp = "tosa.const"() <{values = dense<0> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.avg_pool2d %arg0, %input_zp, %output_zp {acc_type = i32, pad = array<i64: 1, 1, 1, 1>, kernel = array<i64: 4, 4>, stride = array<i64: 1, 1>} : (tensor<1x6x34x62xi8>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x5x33x62xi8>
  return %0 : tensor<1x5x33x62xi8>
}

// -----

// CHECK-LABEL: @avg_pool_dyn
func.func @avg_pool_dyn(%arg0: tensor<?x6x34x62xf32>) -> (tensor<?x5x33x62xf32>) {
  // CHECK: %[[C0:.+]] = arith.constant 0 : index
  // CHECK: %[[BATCH:.+]] = tensor.dim %arg0, %[[C0]]
  // CHECK: %[[F0:.+]] = arith.constant 0.000000e+00 : f32
  // CHECK: %[[PADDED:.+]] = tensor.pad %arg0 low[0, 1, 1, 0] high[0, 1, 1, 0]
  // CHECK:   tensor.yield %[[F0]]
  // CHECK: %[[F0:.+]] = arith.constant 0.000000e+00 : f32
  // CHECK: %[[EMPTY:.+]] = tensor.empty(%[[BATCH]]) : tensor<?x5x33x62xf32>
  // CHECK: %[[FILL:.+]] = linalg.fill ins(%[[F0]] : f32) outs(%[[EMPTY]] : tensor<?x5x33x62xf32>)
  // CHECK: %[[KERNEL:.+]] = tensor.empty() : tensor<4x4xf32>
  // CHECK: %[[POOL:.+]] = linalg.pooling_nhwc_sum
  // CHECK-SAME: dilations = dense<1> : vector<2xi64>, strides = dense<1> : vector<2xi64>
  // CHECK-SAME: ins(%[[PADDED]], %[[KERNEL]] : tensor<?x8x36x62xf32>, tensor<4x4xf32>)
  // CHECK-SAME: outs(%[[FILL]] : tensor<?x5x33x62xf32>) -> tensor<?x5x33x62xf32>
  // CHECK: %[[EMPTY:.+]] = tensor.empty(%[[BATCH]]) : tensor<?x5x33x62xf32>
  // CHECK: %[[GENERIC:.+]] = linalg.generic
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %output_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.avg_pool2d %arg0, %input_zp, %output_zp {acc_type = f32, pad = array<i64: 1, 1, 1, 1>, kernel = array<i64: 4, 4>, stride = array<i64: 1, 1>} : (tensor<?x6x34x62xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<?x5x33x62xf32>
  return %0 : tensor<?x5x33x62xf32>
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3) -> (0)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @conv2d_scalar_bias_f32
func.func @conv2d_scalar_bias_f32(%input: tensor<1x49x42x27xf32>, %weights: tensor<28x3x3x27xf32>, %bias: tensor<1xf32>) -> () {
  // CHECK: %[[INIT:.+]] = tensor.empty() : tensor<1x45x40x28xf32>
  // CHECK: %[[BROADCAST:.+]] = linalg.generic {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2 : tensor<1xf32>) outs(%[[INIT]] : tensor<1x45x40x28xf32>) {
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 1>} : (tensor<1x49x42x27xf32>, tensor<28x3x3x27xf32>, tensor<1xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x45x40x28xf32>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @conv2d_i8
func.func @conv2d_i8(%input: tensor<1x49x42x27xi8>, %weights: tensor<28x1x1x27xi8>, %bias: tensor<28xi8>) -> () {
  // HWCF: %[[TRANSPOSE:.+]] = linalg.transpose ins(%arg1 : tensor<28x1x1x27xi8>) outs(%[[TRANSPOSEDINIT:.+]] : tensor<1x1x27x28xi8>) permutation = [1, 2, 3, 0]
  // CHECK: %[[INIT:.+]] = tensor.empty() : tensor<1x49x42x28xi32>
  // CHECK: %[[BROADCAST:.+]] = linalg.generic {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2 : tensor<28xi8>) outs(%[[INIT]] : tensor<1x49x42x28xi32>) {
  // CHECK:   arith.extsi
  // CHECK:   linalg.yield
  // CHECK: } -> tensor<1x49x42x28xi32>
  // CHECK: linalg.conv_2d_nhwc_fhwc {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<1x49x42x27xi8>, tensor<28x1x1x27xi8>) outs(%[[BROADCAST]] : tensor<1x49x42x28xi32>) -> tensor<1x49x42x28xi32>
  // HWCF: linalg.conv_2d_nhwc_hwcf {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %[[TRANSPOSE]] : tensor<1x49x42x27xi8>, tensor<1x1x27x28xi8>) outs(%{{[a-zA-Z0-9_]*}} : tensor<1x49x42x28xi32>) -> tensor<1x49x42x28xi32>

  %input_zp = "tosa.const"() <{values = dense<0> : tensor<1xi8>}> : () -> tensor<1xi8>
  %weight_zp = "tosa.const"() <{values = dense<0> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = i32, dilation = array<i64: 2, 1>, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>} : (tensor<1x49x42x27xi8>, tensor<28x1x1x27xi8>, tensor<28xi8>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x49x42x28xi32>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @conv2d_f32
func.func @conv2d_f32(%input: tensor<1x49x42x27xf32>, %weights: tensor<28x3x3x27xf32>, %bias: tensor<28xf32>) -> () {
  // HWCF: %[[TRANSPOSE:.+]] =  linalg.transpose ins(%arg1 : tensor<28x3x3x27xf32>) outs(%[[TRANSPOSEDINIT:.+]] : tensor<3x3x27x28xf32>) permutation = [1, 2, 3, 0]

  // CHECK: %[[INIT:.+]] = tensor.empty() : tensor<1x45x40x28xf32>
  // CHECK: %[[BROADCAST:.+]] = linalg.generic {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2 : tensor<28xf32>) outs(%[[INIT]] : tensor<1x45x40x28xf32>) {
  // CHECK:   linalg.yield
  // CHECK: } -> tensor<1x45x40x28xf32>
  // CHECK: linalg.conv_2d_nhwc_fhwc {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<1x49x42x27xf32>, tensor<28x3x3x27xf32>) outs(%[[BROADCAST]] : tensor<1x45x40x28xf32>) -> tensor<1x45x40x28xf32>

  // HWCF: linalg.conv_2d_nhwc_hwcf {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %[[TRANSPOSE]] : tensor<1x49x42x27xf32>, tensor<3x3x27x28xf32>) outs(%{{[a-zA-Z0-9_]*}} : tensor<1x45x40x28xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 1>} : (tensor<1x49x42x27xf32>, tensor<28x3x3x27xf32>, tensor<28xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x45x40x28xf32>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @conv2d_dyn
func.func @conv2d_dyn(%input: tensor<?x49x42x27xf32>, %weights: tensor<28x3x3x27xf32>, %bias: tensor<28xf32>) -> () {
  // CHECK: %[[C0:.+]] = arith.constant 0 : index
  // CHECK: %[[BATCH:.+]] = tensor.dim %arg0, %[[C0]] : tensor<?x49x42x27xf32>
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[BATCH]]) : tensor<?x45x40x28xf32>
  // CHECK: %[[BROADCAST:.+]] = linalg.generic {indexing_maps = [#map, #map1], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2 : tensor<28xf32>) outs(%[[INIT]] : tensor<?x45x40x28xf32>) {
  // CHECK: ^bb0(%[[IN:.+]]: f32, %{{.+}}: f32):
  // CHECK:   linalg.yield %[[IN]] : f32
  // CHECK: } -> tensor<?x45x40x28xf32>
  // CHECK: linalg.conv_2d_nhwc_fhwc {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<?x49x42x27xf32>, tensor<28x3x3x27xf32>) outs(%[[BROADCAST]] : tensor<?x45x40x28xf32>) -> tensor<?x45x40x28xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 1>} : (tensor<?x49x42x27xf32>, tensor<28x3x3x27xf32>, tensor<28xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<?x45x40x28xf32>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @conv2d_dyn_w_h
func.func @conv2d_dyn_w_h(%input: tensor<1x?x?x27xf32>, %weights: tensor<28x3x3x27xf32>, %bias: tensor<28xf32>) -> () {
  // Computing output height
  // CHECK: %[[C1:.+]] = arith.constant 1
  // CHECK: %[[H:.+]] = tensor.dim %arg0, %[[C1]]
  // CHECK: %[[C1_0:.+]] = arith.constant 1
  // CHECK: %[[KH:.+]] = tensor.dim %arg1, %[[C1_0]]
  // CHECK: %[[ONE:.+]] = arith.constant 1 : index
  // CHECK: %[[PAD_0:.+]] = arith.constant 0 : index
  // CHECK: %[[ADD_PAD_0:.+]] = arith.addi %[[H]], %[[PAD_0]] : index
  // CHECK: %[[PAD_1:.+]] = arith.constant 0 : index
  // CHECK: %[[ADD_PAD_1:.+]] = arith.addi %[[ADD_PAD_0]], %[[PAD_1]] : index
  // CHECK: %[[SUB_ONE:.+]] = arith.subi %[[KH]], %[[ONE]] : index
  // CHECK: %[[DIL_H:.+]] = arith.constant 2 : index
  // CHECK: %[[DILATED:.+]] = arith.muli %[[DIL_H]], %[[SUB_ONE]] : index
  // CHECK: %[[ADD_ONE:.+]] = arith.addi %[[DILATED]], %[[ONE]] : index
  // CHECK: %[[SUBTRACTED:.+]] = arith.subi %[[ADD_PAD_1]], %[[ADD_ONE]] : index
  // CHECK: %[[STRIDE_H:.+]] = arith.constant 1 : index
  // CHECK: %[[DIVIDED:.+]] = arith.divui %[[SUBTRACTED]], %[[STRIDE_H]] : index
  // CHECK: %[[H_OUT:.+]] = arith.addi %[[DIVIDED]], %[[ONE]] : index

  // Computing output width
  // CHECK: %[[C2:.+]] = arith.constant 2
  // CHECK: %[[W:.+]] = tensor.dim %arg0, %[[C2]]
  // CHECK: %[[C2_0:.+]] = arith.constant 2
  // CHECK: %[[KW:.+]] = tensor.dim %arg1, %[[C2_0]]
  // CHECK: %[[ONE_0:.+]] = arith.constant 1 : index
  // CHECK: %[[PAD_2:.+]] = arith.constant 0 : index
  // CHECK: %[[ADD_PAD_2:.+]] = arith.addi %[[W]], %[[PAD_2]] : index
  // CHECK: %[[PAD_3:.+]] = arith.constant 0 : index
  // CHECK: %[[ADD_PAD_3:.+]] = arith.addi %[[ADD_PAD_2]], %[[PAD_3]] : index
  // CHECK: %[[SUB_ONE_0:.+]] = arith.subi %[[KW]], %[[ONE_0]] : index
  // CHECK: %[[DIL_W:.+]] = arith.constant 1 : index
  // CHECK: %[[DILATED_0:.+]] = arith.muli %[[DIL_W]], %[[SUB_ONE_0]] : index
  // CHECK: %[[ADD_ONE_0:.+]] = arith.addi %[[DILATED_0]], %[[ONE_0]] : index
  // CHECK: %[[SUBTRACTED_0:.+]] = arith.subi %[[ADD_PAD_3]], %[[ADD_ONE_0]] : index
  // CHECK: %[[STRIDE_W:.+]] = arith.constant 1 : index
  // CHECK: %[[DIVIDED_0:.+]] = arith.divui %[[SUBTRACTED_0]], %[[STRIDE_W]] : index
  // CHECK: %[[W_OUT:.+]] = arith.addi %[[DIVIDED_0]], %[[ONE_0]] : index

  // Running convolution
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[H_OUT]], %[[W_OUT]]) : tensor<1x?x?x28xf32>
  // CHECK: %[[BROADCAST:.+]] = linalg.generic {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2 : tensor<28xf32>) outs(%[[INIT]] : tensor<1x?x?x28xf32>) {
  // CHECK:   linalg.yield
  // CHECK: } -> tensor<1x?x?x28xf32>
  // CHECK: linalg.conv_2d_nhwc_fhwc {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<1x?x?x27xf32>, tensor<28x3x3x27xf32>) outs(%[[BROADCAST]] : tensor<1x?x?x28xf32>) -> tensor<1x?x?x28xf32>

  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 1>} : (tensor<1x?x?x27xf32>, tensor<28x3x3x27xf32>, tensor<28xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x?x?x28xf32>
  return
}

// -----

// CHECK: [[$MAP1:.+]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: [[$MAP2:.+]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

func.func @conv2d_dyn_output(%input: tensor<2x6x5x4xf32>, %weights: tensor<4x3x3x4xf32>, %bias: tensor<4xf32>) {
  // %[[C0:.+]] = arith.constant 0 : index
  // %[[DIM0:.+]] = tensor.dim %input, %[[C0]] : tensor<2x6x5x4xf32>
  // %[[INIT_CONV:.+]] = tensor.empty(%[[DIM0]]) : tensor<?x4x3x4xf32>
  // %[[ZERO:.+]] = arith.constant 0.000000e+00 : f32
  // %[[FILL:.+]] = linalg.fill
  // %[[INIT_GENERIC:.+]] = tensor.empty([[DIM0]]) : tensor<?x4x3x4xf32>

  // %[[CONV:.+]] = linalg.conv_2d_nhwc_fhwc {dilations = dense<1> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<2x6x5x4xf32>, tensor<4x3x3x4xf32>) outs(%[[INIT_CONV]] : tensor<?x4x3x4xf32>) -> tensor<?x4x3x4xf32>
  // linalg.generic {indexing_maps = [#[[MAP1]], #[[MAP2]], #[[MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, %[[CONV]] : tensor<4xf32>, tensor<?x4x3x4xf32>) outs(%[[INIT_GENERIC]] : tensor<?x4x3x4xf32>) {
  //   %[[ADD:.+]] = arith.addf
  //   linalg.yield %[[ADD]] : f32
  // } -> tensor<?x4x3x4xf32>

  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, dilation = array<i64: 1, 1>, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>} : (tensor<2x6x5x4xf32>, tensor<4x3x3x4xf32>, tensor<4xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<?x4x3x4xf32>
  return
}

// -----

// CHECK-LABEL: @conv2d_padded_f32
func.func @conv2d_padded_f32(%input: tensor<1x47x40x28xf32>, %weights: tensor<28x3x3x28xf32>, %bias: tensor<28xf32>) -> () {
  // CHECK: %[[C0:.+]] = arith.constant 0
  // CHECK: tensor.pad %arg0 low[0, 1, 1, 0] high[0, 1, 1, 0]
  // CHECK:   tensor.yield %[[C0]]
  // CHECK: linalg.conv_2d_nhwc_fhwc
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 1, 1, 1, 1>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 1>}
    : (tensor<1x47x40x28xf32>, tensor<28x3x3x28xf32>, tensor<28xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x45x40x28xf32>
  return
}

// -----

// CHECK-LABEL: @conv2d_quant
func.func @conv2d_quant(%arg0 : tensor<1x12x12x1xi8>, %arg1 : tensor<1024x3x3x1xi8>, %arg2 : tensor<1024xi32>) -> () {
  // CHECK:   %[[C22:.+]] = arith.constant -22
  // CHECK: tensor.pad %arg0 low[0, 1, 1, 0] high[0, 1, 1, 0]
  // CHECK:   tensor.yield %[[C22]]
  // CHECK: linalg.conv_2d_nhwc_fhwc_q
  %input_zp = "tosa.const"() <{values = dense<-22> : tensor<1xi8>}> : () -> tensor<1xi8>
  %weight_zp = "tosa.const"() <{values = dense<42> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = i32, dilation = array<i64: 1, 1>, pad = array<i64: 1, 1, 1, 1>, stride = array<i64: 1, 1>}
    : (tensor<1x12x12x1xi8>, tensor<1024x3x3x1xi8>, tensor<1024xi32>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x12x12x1024xi32>
  return
}

// -----

// CHECK-LABEL: @conv2d_f16_f32_acc
func.func @conv2d_f16_f32_acc(%input: tensor<1x49x42x27xf16>, %weights: tensor<28x3x3x27xf16>, %bias: tensor<28xf16>) -> () {
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  // CHECK: linalg.generic {{{.*}}} ins(%{{.*}} : tensor<28xf16>) outs(%{{.*}} : tensor<1x45x40x28xf32>)
  // CHECK: arith.extf %{{.*}} : f16 to f32
  // CHECK: %[[CONV:.*]] = linalg.conv_2d_nhwc_fhwc {{{.*}}} ins(%{{.*}}, %{{.*}} : tensor<1x49x42x27xf16>, tensor<28x3x3x27xf16>) outs(%{{.*}} : tensor<1x45x40x28xf32>) -> tensor<1x45x40x28xf32>
  // CHECK: tosa.cast %[[CONV]] : (tensor<1x45x40x28xf32>) -> tensor<1x45x40x28xf16>
  %0 = tosa.conv2d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 1>} : (tensor<1x49x42x27xf16>, tensor<28x3x3x27xf16>, tensor<28xf16>, tensor<1xf16>, tensor<1xf16>) -> tensor<1x45x40x28xf16>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_conv
func.func @depthwise_conv(%arg0 : tensor<1x7x5x3xf32>, %arg1 : tensor<3x1x3x11xf32>, %arg2 : tensor<33xf32>) -> () {
  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[CST0:%.+]] = arith.constant 0
  // CHECK: [[FILL:%.+]] = linalg.fill ins([[CST0]]{{.*}}outs([[INIT]]
  // CHECK: [[OUT:%.+]] = tensor.empty()
  // CHECK: [[DEPTH:%.+]] = linalg.depthwise_conv_2d_nhwc_hwcm {dilations = dense<1> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<1x7x5x3xf32>, tensor<3x1x3x11xf32>) outs([[FILL]] : tensor<1x5x5x3x11xf32>)
  // CHECK: [[COLLAPSED:%.+]] = tensor.collapse_shape [[DEPTH]] {{\[}}[0], [1], [2], [3, 4]]
  // CHECK: [[BIAS:%.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, [[COLLAPSED]] : tensor<33xf32>, tensor<1x5x5x33xf32>) outs([[OUT]] : tensor<1x5x5x33xf32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: f32, %[[ARG4:[0-9a-zA-Z_]+]]: f32, %[[ARG5:[0-9a-zA-Z_]+]]: f32):
  // CHECK:   [[ADD:%.+]] = arith.addf %[[ARG3]], %[[ARG4]] : f32
  // CHECK:   linalg.yield [[ADD]] : f32
  // CHECK: } -> tensor<1x5x5x33xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %2 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 1, 1> } : (tensor<1x7x5x3xf32>, tensor<3x1x3x11xf32>, tensor<33xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x5x5x33xf32>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (0)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_conv_scalar_bias
func.func @depthwise_conv_scalar_bias(%arg0 : tensor<1x7x5x3xf32>, %arg1 : tensor<3x1x3x11xf32>, %arg2 : tensor<1xf32>) -> () {
  // CHECK: [[BIAS:%.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, %{{.*}} : tensor<1xf32>, tensor<1x5x5x33xf32>) outs(%{{.*}} : tensor<1x5x5x33xf32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: f32, %[[ARG4:[0-9a-zA-Z_]+]]: f32, %{{.*}}: f32):
  // CHECK:   [[ADD:%.+]] = arith.addf %[[ARG3]], %[[ARG4]] : f32
  // CHECK:   linalg.yield [[ADD]] : f32
  // CHECK: } -> tensor<1x5x5x33xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %2 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 1, 1> } : (tensor<1x7x5x3xf32>, tensor<3x1x3x11xf32>, tensor<1xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x5x5x33xf32>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_conv_dyn
func.func @depthwise_conv_dyn(%arg0 : tensor<?x7x5x3xf32>, %arg1 : tensor<3x1x3x11xf32>, %arg2 : tensor<33xf32>) -> () {
  // CHECK: %[[C0:.+]] = arith.constant 0
  // CHECK: %[[BATCH:.+]] = tensor.dim %arg0, %[[C0]]
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[BATCH]])
  // CHECK: %[[CST0:.+]] = arith.constant 0
  // CHECK: %[[FILL:.+]] = linalg.fill
  // CHECK: %[[OUT:.+]] = tensor.empty(%[[BATCH]])
  // CHECK: %[[DEPTH:.+]] = linalg.depthwise_conv_2d_nhwc_hwcm {dilations = dense<1> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<?x7x5x3xf32>, tensor<3x1x3x11xf32>) outs(%[[FILL]] : tensor<?x5x5x3x11xf32>)
  // CHECK: %[[COLLAPSED:.+]] = tensor.collapse_shape %[[DEPTH]] {{\[}}[0], [1], [2], [3, 4]]
  // CHECK: %[[BIAS:.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, %[[COLLAPSED]] : tensor<33xf32>, tensor<?x5x5x33xf32>) outs(%[[OUT]] : tensor<?x5x5x33xf32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: f32, %[[ARG4:[0-9a-zA-Z_]+]]: f32, %[[ARG5:[0-9a-zA-Z_]+]]: f32):
  // CHECK:   %[[ADD:.+]] = arith.addf %[[ARG3]], %[[ARG4]] : f32
  // CHECK:   linalg.yield %[[ADD]] : f32
  // CHECK: } -> tensor<?x5x5x33xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %2 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 1, 1> } : (tensor<?x7x5x3xf32>, tensor<3x1x3x11xf32>, tensor<33xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<?x5x5x33xf32>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_conv_strides
func.func @depthwise_conv_strides(%arg0 : tensor<1x11x9x3xf32>, %arg1 : tensor<3x1x3x11xf32>, %arg2 : tensor<33xf32>) -> () {
  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[CST0:%.+]] = arith.constant 0
  // CHECK: [[FILL:%.+]] = linalg.fill ins([[CST0]]{{.*}}outs([[INIT]]
  // CHECK: [[OUT:%.+]] = tensor.empty()
  // CHECK: [[DEPTH:%.+]] = linalg.depthwise_conv_2d_nhwc_hwcm {dilations = dense<1> : tensor<2xi64>, strides = dense<2> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<1x11x9x3xf32>, tensor<3x1x3x11xf32>) outs([[FILL]] : tensor<1x5x5x3x11xf32>)
  // CHECK: [[COLLAPSED:%.+]] = tensor.collapse_shape [[DEPTH]] {{\[}}[0], [1], [2], [3, 4]]
  // CHECK: [[BIAS:%.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, [[COLLAPSED]] : tensor<33xf32>, tensor<1x5x5x33xf32>) outs([[OUT]] : tensor<1x5x5x33xf32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: f32, %[[ARG4:[0-9a-zA-Z_]+]]: f32, %[[ARG5:[0-9a-zA-Z_]+]]: f32):
  // CHECK:   [[ADD:%.+]] = arith.addf %[[ARG3]], %[[ARG4]] : f32
  // CHECK:   linalg.yield [[ADD]] : f32
  // CHECK: } -> tensor<1x5x5x33xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %2 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 2, 2>, dilation = array<i64: 1, 1> } : (tensor<1x11x9x3xf32>, tensor<3x1x3x11xf32>, tensor<33xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x5x5x33xf32>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_conv_quant
func.func @depthwise_conv_quant(%arg0 : tensor<1x12x12x4xi8>, %arg1 : tensor<3x3x4x128xi8>, %arg2 : tensor<512xi32>) -> () {
  // CHECK: [[PADV:%.+]] = arith.constant -128
  // CHECK: [[PAD:%.+]] = tensor.pad %arg0 low[0, 1, 1, 0] high[0, 1, 1, 0]
  // CHECK:   tensor.yield [[PADV]]

  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[CST0:%.+]] = arith.constant 0
  // CHECK: [[FILL:%.+]] = linalg.fill ins([[CST0]]{{.*}}outs([[INIT]]
  // CHECK: [[OUT:%.+]] = tensor.empty()
  // CHECK: [[C128:%.+]] = arith.constant -128
  // CHECK: [[C42:%.+]] = arith.constant 42
  // CHECK: [[DEPTH:%.+]] = linalg.depthwise_conv_2d_nhwc_hwcm_q {dilations = dense<1> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins([[PAD]], %arg1, [[C128]], [[C42]] : tensor<1x14x14x4xi8>, tensor<3x3x4x128xi8>, i32, i32) outs([[FILL]] : tensor<1x12x12x4x128xi32>)
  // CHECK: [[COLLAPSED:%.+]] = tensor.collapse_shape [[DEPTH]] {{\[}}[0], [1], [2], [3, 4]]
  // CHECK: [[BIAS:%.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, [[COLLAPSED]] : tensor<512xi32>, tensor<1x12x12x512xi32>) outs([[OUT]] : tensor<1x12x12x512xi32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: i32, %[[ARG4:[0-9a-zA-Z_]+]]: i32, %[[ARG5:[0-9a-zA-Z_]+]]: i32):
  // CHECK:   [[ADD:%.+]] = arith.addi %[[ARG3]], %[[ARG4]] : i32
  // CHECK:   linalg.yield [[ADD]] : i32
  // CHECK: } -> tensor<1x12x12x512xi32>
  %input_zp = "tosa.const"() <{values = dense<-128> : tensor<1xi8>}> : () -> tensor<1xi8>
  %weight_zp = "tosa.const"() <{values = dense<42> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = i32, pad = array<i64: 1, 1, 1, 1>, stride = array<i64: 1, 1>, dilation = array<i64: 1, 1> } : (tensor<1x12x12x4xi8>, tensor<3x3x4x128xi8>, tensor<512xi32>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x12x12x512xi32>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_conv_quant_dilations
func.func @depthwise_conv_quant_dilations(%arg0 : tensor<1x14x14x4xi8>, %arg1 : tensor<3x3x4x128xi8>, %arg2 : tensor<512xi32>) -> () {
  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[CST0:%.+]] = arith.constant 0
  // CHECK: [[FILL:%.+]] = linalg.fill ins([[CST0]]{{.*}}outs([[INIT]]
  // CHECK: [[OUT:%.+]] = tensor.empty()
  // CHECK: [[C128:%.+]] = arith.constant -128
  // CHECK: [[C42:%.+]] = arith.constant 42
  // CHECK: [[DEPTH:%.+]] = linalg.depthwise_conv_2d_nhwc_hwcm_q {dilations = dense<2> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1, [[C128]], [[C42]] : tensor<1x14x14x4xi8>, tensor<3x3x4x128xi8>, i32, i32) outs([[FILL]] : tensor<1x10x10x4x128xi32>)
  // CHECK: [[COLLAPSED:%.+]] = tensor.collapse_shape [[DEPTH]] {{\[}}[0], [1], [2], [3, 4]]
  // CHECK: [[BIAS:%.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, [[COLLAPSED]] : tensor<512xi32>, tensor<1x10x10x512xi32>) outs([[OUT]] : tensor<1x10x10x512xi32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: i32, %[[ARG4:[0-9a-zA-Z_]+]]: i32, %[[ARG5:[0-9a-zA-Z_]+]]: i32):
  // CHECK:   [[ADD:%.+]] = arith.addi %[[ARG3]], %[[ARG4]] : i32
  // CHECK:   linalg.yield [[ADD]] : i32
  // CHECK: } -> tensor<1x10x10x512xi32>
  %input_zp = "tosa.const"() <{values = dense<-128> : tensor<1xi8>}> : () -> tensor<1xi8>
  %weight_zp = "tosa.const"() <{values = dense<42> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = i32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 2, 2> } : (tensor<1x14x14x4xi8>, tensor<3x3x4x128xi8>, tensor<512xi32>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x10x10x512xi32>
  return
}

// CHECK-LABEL: @depthwise_conv2d_dyn_w_h
func.func @depthwise_conv2d_dyn_w_h(%arg0: tensor<2x?x?x3xf32>, %arg1: tensor<3x6x3x5xf32>, %arg2: tensor<15xf32>) {
  // CHECK: arith.addi
  // CHECK: arith.subi
  // CHECK: arith.muli
  // CHECK: arith.divui
  // CHECK: %[[PADDED:.+]] = tensor.pad %arg0 low[0, 1, 3, 0] high[0, 2, 4, 0] {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: index, %[[ARG4:[0-9a-zA-Z_]+]]: index, %[[ARG5:[0-9a-zA-Z_]+]]: index, %[[ARG6:[0-9a-zA-Z_]+]]: index):
  // CHECK: tensor.yield %cst : f32
  // CHECK:  } : tensor<2x?x?x3xf32> to tensor<2x?x?x3xf32>
  // CHECK: %[[CONV:.+]] = linalg.depthwise_conv_2d_nhwc_hwcm {dilations = dense<[2, 1]> : tensor<2xi64>, strides = dense<[1, 2]> : tensor<2xi64>} ins(%[[PADDED]], %arg1 : tensor<2x?x?x3xf32>, tensor<3x6x3x5xf32>) outs(%{{.*}} : tensor<2x?x?x3x5xf32>) -> tensor<2x?x?x3x5xf32>
  // CHECK: %[[COLLAPSED:.+]] = tensor.collapse_shape %[[CONV]] {{\[}}[0], [1], [2], [3, 4]]
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 1, 2, 3, 4>, dilation = array<i64: 2, 1>, stride = array<i64: 1, 2>} : (tensor<2x?x?x3xf32>, tensor<3x6x3x5xf32>, tensor<15xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<2x?x?x15xf32>
  return
}

// -----

// CHECK: #[[$MAP0:.*]] = affine_map<(d0, d1, d2, d3) -> (d3)>
// CHECK: #[[$MAP1:.*]] = affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>

// CHECK-LABEL: @depthwise_int_conv_zero_zp
func.func @depthwise_int_conv_zero_zp(%arg0 : tensor<1x7x5x3xi8>, %arg1 : tensor<3x1x3x11xi8>, %arg2 : tensor<33xi32>) -> () {
  // CHECK: [[INIT:%.+]] = tensor.empty()
  // CHECK: [[CST0:%.+]] = arith.constant 0
  // CHECK: [[FILL:%.+]] = linalg.fill ins([[CST0]]{{.*}}outs([[INIT]]
  // CHECK: [[OUT:%.+]] = tensor.empty()
  // CHECK: [[DEPTH:%.+]] = linalg.depthwise_conv_2d_nhwc_hwcm {dilations = dense<1> : tensor<2xi64>, strides = dense<1> : tensor<2xi64>} ins(%arg0, %arg1 : tensor<1x7x5x3xi8>, tensor<3x1x3x11xi8>) outs([[FILL]] : tensor<1x5x5x3x11xi32>)
  // CHECK: [[COLLAPSED:%.+]] = tensor.collapse_shape [[DEPTH]] {{\[}}[0], [1], [2], [3, 4]]
  // CHECK: [[BIAS:%.+]] = linalg.generic {indexing_maps = [#[[$MAP0]], #[[$MAP1]], #[[$MAP1]]], iterator_types = ["parallel", "parallel", "parallel", "parallel"]} ins(%arg2, [[COLLAPSED]] : tensor<33xi32>, tensor<1x5x5x33xi32>) outs([[OUT]] : tensor<1x5x5x33xi32>) {
  // CHECK: ^bb0(%[[ARG3:[0-9a-zA-Z_]+]]: i32, %[[ARG4:[0-9a-zA-Z_]+]]: i32, %[[ARG5:[0-9a-zA-Z_]+]]: i32):
  // CHECK:   [[ADD:%.+]] = arith.addi %[[ARG3]], %[[ARG4]] : i32
  // CHECK:   linalg.yield [[ADD]] : i32
  // CHECK: } -> tensor<1x5x5x33xi32>
  %input_zp = "tosa.const"() <{values = dense<0> : tensor<1xi8>}> : () -> tensor<1xi8>
  %weight_zp = "tosa.const"() <{values = dense<0> : tensor<1xi8>}> : () -> tensor<1xi8>
  %2 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = i32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 1, 1> } : (tensor<1x7x5x3xi8>, tensor<3x1x3x11xi8>, tensor<33xi32>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x5x5x33xi32>
  return
}

// -----

// CHECK-LABEL: @depthwise_conv2d_f16_f32_acc
func.func @depthwise_conv2d_f16_f32_acc(%arg0 : tensor<1x7x5x3xf16>, %arg1 : tensor<3x1x3x11xf16>, %arg2 : tensor<33xf16>) -> () {
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  // CHECK: %[[CONV:.*]] = linalg.depthwise_conv_2d_nhwc_hwcm {{{.*}}} ins(%{{.*}}, %{{.*}} : tensor<1x7x5x3xf16>, tensor<3x1x3x11xf16>) outs(%{{.*}} : tensor<1x5x5x3x11xf32>) -> tensor<1x5x5x3x11xf32>
  // CHECK: tosa.cast %[[CONV]] : (tensor<1x5x5x3x11xf32>) -> tensor<1x5x5x3x11xf16>
  %2 = tosa.depthwise_conv2d %arg0, %arg1, %arg2, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0>, stride = array<i64: 1, 1>, dilation = array<i64: 1, 1> } : (tensor<1x7x5x3xf16>, tensor<3x1x3x11xf16>, tensor<33xf16>, tensor<1xf16>, tensor<1xf16>) -> tensor<1x5x5x33xf16>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3, d4) -> (d4)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3, d4) -> (d0, d1, d2, d3, d4)>

// CHECK-LABEL: @conv3d_f32
func.func @conv3d_f32(%input: tensor<1x49x48x47x27xf32>, %weights: tensor<43x3x4x5x27xf32>, %bias: tensor<43xf32>) -> () {
  // CHECK-DAG:  %[[TRANSPOSE:.+]] = linalg.transpose ins(%arg1 : tensor<43x3x4x5x27xf32>) outs(%[[TRANSPOSEDINIT:.+]] : tensor<3x4x5x27x43xf32>) permutation = [1, 2, 3, 4, 0]
  // CHECK-DAG:  %[[INIT:.+]] = tensor.empty() : tensor<1x47x45x43x43xf32>
  // CHECK:      %[[BROADCAST:.+]] = linalg.generic
  // CHECK-SAME: {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel", "parallel"]}
  // CHECK-SAME: ins(%arg2 : tensor<43xf32>) outs(%[[INIT]] : tensor<1x47x45x43x43xf32>) {
  // CHECK:      ^bb0(%[[IN:.+]]: f32, %[[OUT:.+]]: f32):
  // CHECK:        linalg.yield %[[IN]] : f32
  // CHECK:      } -> tensor<1x47x45x43x43xf32>
  // CHECK:      linalg.conv_3d_ndhwc_dhwcf
  // CHECK-SAME: {dilations = dense<1> : tensor<3xi64>, strides = dense<1> : tensor<3xi64>}
  // CHECK-SAME: ins(%arg0, %[[TRANSPOSE]] : tensor<1x49x48x47x27xf32>, tensor<3x4x5x27x43xf32>)
  // CHECK-SAME: outs(%[[BROADCAST]] : tensor<1x47x45x43x43xf32>) -> tensor<1x47x45x43x43xf32>
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv3d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0, 0, 0>, stride = array<i64: 1, 1, 1>, dilation = array<i64: 1, 1, 1>} : (tensor<1x49x48x47x27xf32>, tensor<43x3x4x5x27xf32>, tensor<43xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x47x45x43x43xf32>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3, d4) -> (0)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3, d4) -> (d0, d1, d2, d3, d4)>

// CHECK-LABEL: @conv3d_scalar_bias_f32
func.func @conv3d_scalar_bias_f32(%input: tensor<1x49x48x47x27xf32>, %weights: tensor<28x3x4x5x27xf32>, %bias: tensor<1xf32>) -> () {
  // CHECK:  %[[INIT:.+]] = tensor.empty() : tensor<1x47x45x43x28xf32>
  // CHECK:      %[[BROADCAST:.+]] = linalg.generic
  // CHECK-SAME: {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel", "parallel"]}
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf32>}> : () -> tensor<1xf32>
  %0 = tosa.conv3d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0, 0, 0>, stride = array<i64: 1, 1, 1>, dilation = array<i64: 1, 1, 1>} : (tensor<1x49x48x47x27xf32>, tensor<28x3x4x5x27xf32>, tensor<1xf32>, tensor<1xf32>, tensor<1xf32>) -> tensor<1x47x45x43x28xf32>
  return
}

// -----

// CHECK: #[[$MAP1:.+]] = affine_map<(d0, d1, d2, d3, d4) -> (d4)>
// CHECK: #[[$MAP2:.+]] = affine_map<(d0, d1, d2, d3, d4) -> (d0, d1, d2, d3, d4)>

// CHECK-LABEL: @conv3d_i8
func.func @conv3d_i8(%input: tensor<1x49x48x47x27xi8>, %weights: tensor<43x3x4x5x27xi8>, %bias: tensor<43xi32>) -> () {
  // CHECK-DAG:  %[[TRANSPOSE:.+]] = linalg.transpose ins(%arg1 : tensor<43x3x4x5x27xi8>) outs(%[[TRANSPOSEDINIT:.+]] : tensor<3x4x5x27x43xi8>) permutation = [1, 2, 3, 4, 0]
  // CHECK-DAG:  %[[INIT:.+]] = tensor.empty() : tensor<1x47x45x43x43xi32>
  // CHECK:      %[[BROADCAST:.+]] = linalg.generic
  // CHECK-SAME: {indexing_maps = [#[[$MAP1]], #[[$MAP2]]], iterator_types = ["parallel", "parallel", "parallel", "parallel", "parallel"]}
  // CHECK-SAME: ins(%arg2 : tensor<43xi32>)
  // CHECK-SAME: outs(%[[INIT]] : tensor<1x47x45x43x43xi32>) {
  // CHECK:      ^bb0(%[[IN:.+]]: i32, %[[OUT:.+]]: i32):
  // CHECK:        linalg.yield %[[IN]] : i32
  // CHECK:      } -> tensor<1x47x45x43x43xi32>
  // CHECK:      %[[IZP:.+]] = arith.constant -128 : i32
  // CHECK:      %[[FZP:.+]] = arith.constant 42 : i32
  // CHECK:      linalg.conv_3d_ndhwc_dhwcf_q
  // CHECK-SAME: {dilations = dense<1> : tensor<3xi64>, strides = dense<1> : tensor<3xi64>}
  // CHECK-SAME: ins(%arg0, %[[TRANSPOSE]], %[[IZP]], %[[FZP]] : tensor<1x49x48x47x27xi8>, tensor<3x4x5x27x43xi8>, i32, i32)
  // CHECK-SAME: outs(%[[BROADCAST]] : tensor<1x47x45x43x43xi32>) -> tensor<1x47x45x43x43xi32>

  %input_zp = "tosa.const"() <{values = dense<-128> : tensor<1xi8>}> : () -> tensor<1xi8>
  %weight_zp = "tosa.const"() <{values = dense<42> : tensor<1xi8>}> : () -> tensor<1xi8>
  %0 = tosa.conv3d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = i32, pad = array<i64: 0, 0, 0, 0, 0, 0>, stride = array<i64: 1, 1, 1>, dilation = array<i64: 1, 1, 1>} : (tensor<1x49x48x47x27xi8>, tensor<43x3x4x5x27xi8>, tensor<43xi32>, tensor<1xi8>, tensor<1xi8>) -> tensor<1x47x45x43x43xi32>
  return
}

// -----

// CHECK-LABEL: @conv3d_f16_f32_acc
func.func @conv3d_f16_f32_acc(%input: tensor<1x49x48x47x27xf16>, %weights: tensor<43x3x4x5x27xf16>, %bias: tensor<43xf16>) -> () {
  %input_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  %weight_zp = "tosa.const"() <{values = dense<0.0> : tensor<1xf16>}> : () -> tensor<1xf16>
  // CHECK: linalg.generic {{{.*}}} ins(%{{.*}} : tensor<43xf16>) outs(%{{.*}} : tensor<1x47x45x43x43xf32>)
  // CHECK: arith.extf %{{.*}} : f16 to f32
  // CHECK: %[[CONV:.*]] = linalg.conv_3d_ndhwc_dhwcf {{{.*}}} ins(%{{.*}}, %{{.*}} : tensor<1x49x48x47x27xf16>, tensor<3x4x5x27x43xf16>) outs(%{{.*}} : tensor<1x47x45x43x43xf32>) -> tensor<1x47x45x43x43xf32>
  // CHECK: tosa.cast %[[CONV]] : (tensor<1x47x45x43x43xf32>) -> tensor<1x47x45x43x43xf16>
  %0 = tosa.conv3d %input, %weights, %bias, %input_zp, %weight_zp {acc_type = f32, pad = array<i64: 0, 0, 0, 0, 0, 0>, stride = array<i64: 1, 1, 1>, dilation = array<i64: 1, 1, 1>} : (tensor<1x49x48x47x27xf16>, tensor<43x3x4x5x27xf16>, tensor<43xf16>, tensor<1xf16>, tensor<1xf16>) -> tensor<1x47x45x43x43xf16>
  return
}

// -----

// CHECK-LABEL: @test_transpose
// CHECK-SAME: (%[[ARG0:.+]]: tensor<1x2x3xi32>)
func.func @test_transpose(%arg0: tensor<1x2x3xi32>) -> () {
  // CHECK: %[[INIT:.+]] = tensor.empty() : tensor<2x3x1xi32>
  // CHECK: %[[TRANSPOSE:.+]] = linalg.transpose ins(%[[ARG0]] : tensor<1x2x3xi32>) outs(%[[INIT]] : tensor<2x3x1xi32>) permutation = [1, 2, 0]
  %1 = tosa.transpose %arg0 {perms = array<i32: 1, 2, 0>}: (tensor<1x2x3xi32>)  -> tensor<2x3x1xi32>
  return
}

// -----

// CHECK-LABEL: @test_transpose_dyn
// CHECK-SAME: (%[[ARG0:.+]]: tensor<1x?x3x4xi32>)
func.func @test_transpose_dyn(%arg0: tensor<1x?x3x4xi32>) -> () {
  // CHECK: %[[C1:.+]] = arith.constant 1
  // CHECK: %[[DIM:.+]] = tensor.dim %[[ARG0]], %[[C1]]
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[DIM]]) : tensor<?x4x1x3xi32>
  // CHECK: %[[TRANSPOSE:.+]] = linalg.transpose ins(%[[ARG0]] : tensor<1x?x3x4xi32>) outs(%[[INIT]] : tensor<?x4x1x3xi32>) permutation = [1, 3, 0, 2]
  %1 = tosa.transpose %arg0 {perms = array<i32: 1, 3, 0, 2>}: (tensor<1x?x3x4xi32>)  -> tensor<?x4x1x3xi32>
  return
}

// -----

// CHECK-LABEL: @test_transpose_dyn_multiple_2d
// CHECK-SAME: (%[[ARG0:.+]]: tensor<?x?xf32>)
func.func @test_transpose_dyn_multiple_2d(%arg0: tensor<?x?xf32>) -> () {
  // CHECK-DAG: %[[C0:.+]] = arith.constant 0
  // CHECK-DAG: %[[DIM0:.+]] = tensor.dim %[[ARG0]], %[[C0]]
  // CHECK-DAG: %[[C1:.+]] = arith.constant 1
  // CHECK-DAG: %[[DIM1:.+]] = tensor.dim %[[ARG0]], %[[C1]]
  // CHECK: %[[INIT:.+]] = tensor.empty(%[[DIM1]], %[[DIM0]])
  // CHECK: %[[TRANSPOSE:.+]] = linalg.transpose ins(%[[ARG0]] : tensor<?x?xf32>) outs(%[[INIT]] : tensor<?x?xf32>) permutation = [1, 0]
  %1 = tosa.transpose %arg0 {perms = array<i32: 1, 0>}: (tensor<?x?xf32>)  -> tensor<?x?xf32>
  return
}

// -----

// CHECK-LABEL: @test_transpose_dyn_multiple_3d
// CHECK-SAME: (%[[ARG0:.+]]: tensor<?x?x?xf32>)
func.func @test_transpose_dyn_multiple_3d(%arg0: tensor<?x?x?xf32>) {
  // CHECK-DAG: %[[C0:.*]] = arith.constant 0 : index
  // CHECK-DAG: %[[DIM0:.*]] = tensor.dim %[[ARG0]], %[[C0]] : tensor<?x?x?xf32>
  // CHECK-DAG: %[[C1:.*]] = arith.constant 1 : index
  // CHECK-DAG: %[[DIM1:.*]] = tensor.dim %[[ARG0]], %[[C1]] : tensor<?x?x?xf32>
  // CHECK-DAG: %[[C2:.*]] = arith.constant 2 : index
  // CHECK-DAG: %[[DIM2:.*]] = tensor.dim %[[ARG0]], %[[C2]] : tensor<?x?x?xf32>
  // CHECK: %[[INIT:.*]] = tensor.empty(%[[DIM2]], %[[DIM0]], %[[DIM1]]) : tensor<?x?x?xf32>
  // CHECK: %[[TRANSPOSE:.*]] = linalg.transpose ins(%[[ARG0]] : tensor<?x?x?xf32>) outs(%[[INIT]] : tensor<?x?x?xf32>) permutation = [2, 0, 1]
  %1 = "tosa.transpose"(%arg0) {perms = array<i32: 2, 0, 1>} : (tensor<?x?x?xf32>) -> tensor<?x?x?xf32>
  return
}

// -----

// CHECK-LABEL: @max_pool2d_nan_propagate
func.func @max_pool2d_nan_propagate(%arg0: tensor<1x6x34x62xf32>) -> (tensor<1x4x32x62xf32>) {
  // CHECK: linalg.pooling_nhwc_max
  // CHECK-NOT: linalg.generic
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>, nan_mode = "PROPAGATE"} : (tensor<1x6x34x62xf32>) -> tensor<1x4x32x62xf32>
  return %0 : tensor<1x4x32x62xf32>
}

// -----

// CHECK-LABEL: @max_pool2d_nan_ignore_int
func.func @max_pool2d_nan_ignore_int(%arg0: tensor<1x6x34x62xi8>) -> (tensor<1x4x32x62xi8>) {
  // CHECK: linalg.pooling_nhwc_max
  // CHECK-NOT: linalg.generic
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>, nan_mode = "IGNORE"} : (tensor<1x6x34x62xi8>) -> tensor<1x4x32x62xi8>
  return %0: tensor<1x4x32x62xi8>
}

// -----

// CHECK-LABEL: @max_pool2d_nan_ignore
func.func @max_pool2d_nan_ignore(%arg0: tensor<1x6x34x62xf32>) -> (tensor<1x4x32x62xf32>) {
  // CHECK-NOT: linalg.pooling_nhwc_max
  // CHECK: linalg.generic
  // CHECK: arith.maximumf
  // CHECK: arith.cmpf uno
  // CHECK: arith.select
  // CHECK: linalg.yield
  %0 = tosa.max_pool2d %arg0 {pad = array<i64: 0, 0, 0, 0>, kernel = array<i64: 3, 3>, stride = array<i64: 1, 1>, nan_mode = "IGNORE"} : (tensor<1x6x34x62xf32>) -> tensor<1x4x32x62xf32>
  return %0: tensor<1x4x32x62xf32>
}
