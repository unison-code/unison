; ModuleID = 'sphinx3.profile.ptmr_init.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.ptmr_t.43 = type { i8*, double, double, double, double, double, double }

; Function Attrs: norecurse nounwind
define void @ptmr_init(%struct.ptmr_t.43* nocapture %tm) #0 {
  %1 = getelementptr inbounds %struct.ptmr_t.43, %struct.ptmr_t.43* %tm, i32 0, i32 1
  %2 = bitcast double* %1 to i8*
  tail call void @llvm.memset.p0i8.i64(i8* %2, i8 0, i64 32, i32 8, i1 false)
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #1

attributes #0 = { norecurse nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
