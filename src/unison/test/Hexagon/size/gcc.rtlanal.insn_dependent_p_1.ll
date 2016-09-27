; ModuleID = 'gcc.rtlanal.insn_dependent_p_1.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct.rtx_def.150 = type { i32, [1 x %union.rtunion_def.151] }
%union.rtunion_def.151 = type { i32 }

; Function Attrs: nounwind
declare i32 @reg_mentioned_p(%struct.rtx_def.150*, %struct.rtx_def.150*) #0

; Function Attrs: nounwind
define hidden void @insn_dependent_p_1(%struct.rtx_def.150* %x, %struct.rtx_def.150* nocapture readnone %pat, i8* nocapture %data) #0 {
  %1 = bitcast i8* %data to %struct.rtx_def.150**
  %2 = load %struct.rtx_def.150*, %struct.rtx_def.150** %1, align 4
  %3 = icmp eq %struct.rtx_def.150* %2, null
  br i1 %3, label %8, label %4

; <label>:4                                       ; preds = %0
  %5 = tail call i32 @reg_mentioned_p(%struct.rtx_def.150* %x, %struct.rtx_def.150* nonnull %2)
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %8, label %7

; <label>:7                                       ; preds = %4
  store %struct.rtx_def.150* null, %struct.rtx_def.150** %1, align 4
  br label %8

; <label>:8                                       ; preds = %7, %4, %0
  ret void
}

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
