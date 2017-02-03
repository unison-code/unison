; ModuleID = 'hmmer.tophits.AllocFancyAli.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.fancyali_s.23 = type { i8*, i8*, i8*, i8*, i8*, i32, i8*, i8*, i32, i32 }

@.str = external hidden unnamed_addr constant [10 x i8], align 1

declare i8* @sre_malloc(i8*, i32 signext, i32 signext) #0

; Function Attrs: nounwind
define %struct.fancyali_s.23* @AllocFancyAli() #1 {
  %1 = tail call i8* @sre_malloc(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str, i32 0, i32 0), i32 signext 117, i32 signext 40) #3
  %2 = bitcast i8* %1 to %struct.fancyali_s.23*
  %3 = getelementptr inbounds i8, i8* %1, i32 24
  tail call void @llvm.memset.p0i8.i64(i8* %1, i8 0, i64 20, i32 4, i1 false)
  tail call void @llvm.memset.p0i8.i64(i8* %3, i8 0, i64 16, i32 4, i1 false)
  ret %struct.fancyali_s.23* %2
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #2

attributes #0 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { argmemonly nounwind }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
