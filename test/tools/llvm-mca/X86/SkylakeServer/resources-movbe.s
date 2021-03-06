# NOTE: Assertions have been autogenerated by utils/update_mca_test_checks.py
# RUN: llvm-mca -mtriple=x86_64-unknown-unknown -mcpu=skylake-avx512 -instruction-tables < %s | FileCheck %s

movbe  %cx, (%rax)
movbe  (%rax), %cx

movbe  %ecx, (%rax)
movbe  (%rax), %ecx

movbe  %rcx, (%rax)
movbe  (%rax), %rcx

# CHECK:      Instruction Info:
# CHECK-NEXT: [1]: #uOps
# CHECK-NEXT: [2]: Latency
# CHECK-NEXT: [3]: RThroughput
# CHECK-NEXT: [4]: MayLoad
# CHECK-NEXT: [5]: MayStore
# CHECK-NEXT: [6]: HasSideEffects (U)

# CHECK:      [1]    [2]    [3]    [4]    [5]    [6]    Instructions:
# CHECK-NEXT:  3      2     1.00           *            movbew	%cx, (%rax)
# CHECK-NEXT:  2      6     0.50    *                   movbew	(%rax), %cx
# CHECK-NEXT:  3      2     1.00           *            movbel	%ecx, (%rax)
# CHECK-NEXT:  2      6     0.50    *                   movbel	(%rax), %ecx
# CHECK-NEXT:  3      2     1.00           *            movbeq	%rcx, (%rax)
# CHECK-NEXT:  2      6     0.50    *                   movbeq	(%rax), %rcx

# CHECK:      Resources:
# CHECK-NEXT: [0]   - SKXDivider
# CHECK-NEXT: [1]   - SKXFPDivider
# CHECK-NEXT: [2]   - SKXPort0
# CHECK-NEXT: [3]   - SKXPort1
# CHECK-NEXT: [4]   - SKXPort2
# CHECK-NEXT: [5]   - SKXPort3
# CHECK-NEXT: [6]   - SKXPort4
# CHECK-NEXT: [7]   - SKXPort5
# CHECK-NEXT: [8]   - SKXPort6
# CHECK-NEXT: [9]   - SKXPort7

# CHECK:      Resource pressure per iteration:
# CHECK-NEXT: [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]
# CHECK-NEXT:  -      -      -     3.00   2.50   2.50   3.00   3.00    -     1.00

# CHECK:      Resource pressure by instruction:
# CHECK-NEXT: [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    Instructions:
# CHECK-NEXT:  -      -      -     0.50   0.33   0.33   1.00   0.50    -     0.33   movbew	%cx, (%rax)
# CHECK-NEXT:  -      -      -     0.50   0.50   0.50    -     0.50    -      -     movbew	(%rax), %cx
# CHECK-NEXT:  -      -      -     0.50   0.33   0.33   1.00   0.50    -     0.33   movbel	%ecx, (%rax)
# CHECK-NEXT:  -      -      -     0.50   0.50   0.50    -     0.50    -      -     movbel	(%rax), %ecx
# CHECK-NEXT:  -      -      -     0.50   0.33   0.33   1.00   0.50    -     0.33   movbeq	%rcx, (%rax)
# CHECK-NEXT:  -      -      -     0.50   0.50   0.50    -     0.50    -      -     movbeq	(%rax), %rcx
