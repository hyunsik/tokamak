; ModuleID = 'llvm-ir/llvm-ir.cc'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define double @cos1(double %x) #0 {
entry:
  ret double 1.980410e+03
}

define float @f1(float) {
EntryBlock:
  %1 = call float @llvm.sin.f32(float %0)
  ret float %1
}

; Function Attrs: nounwind readnone
declare float @llvm.sin.f32(float) #0
