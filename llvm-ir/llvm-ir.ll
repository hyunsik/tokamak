; ModuleID = 'llvm-ir/llvm-ir.cc'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define double @cos1(double %x) #0 {
entry:
  ret double 1.980410e+03
}

; Function Attrs: nounwind readnone uwtable
define zeroext i1 @_ZN5Field3abcEv() #0 align 2 {
entry:
  ret i1 true
}

attributes #0 = { nounwind readnone uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.6.0 (tags/RELEASE_360/final)"}
