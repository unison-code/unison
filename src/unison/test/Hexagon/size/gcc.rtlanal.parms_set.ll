; ModuleID = 'gcc.rtlanal.parms_set.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct.rtx_def.365 = type { i32, [1 x %union.rtunion_def.366] }
%union.rtunion_def.366 = type { i32 }

; Function Attrs: norecurse nounwind
define hidden void @parms_set(%struct.rtx_def.365* nocapture readonly %x, %struct.rtx_def.365* nocapture readnone %pat, i8* nocapture %data) #0 {
  %1 = getelementptr inbounds %struct.rtx_def.365, %struct.rtx_def.365* %x, i32 0, i32 0
  %2 = load i32, i32* %1, align 4
  %3 = and i32 %2, 255
  %4 = icmp eq i32 %3, 61
  br i1 %4, label %5, label %22

; <label>:5                                       ; preds = %0
  %6 = getelementptr inbounds %struct.rtx_def.365, %struct.rtx_def.365* %x, i32 0, i32 1, i32 0, i32 0
  %7 = load i32, i32* %6, align 4
  %8 = icmp ult i32 %7, 53
  br i1 %8, label %9, label %22

; <label>:9                                       ; preds = %5
  %10 = getelementptr inbounds i8, i8* %data, i32 4
  %11 = bitcast i8* %10 to i32*
  %12 = load i32, i32* %11, align 4
  %13 = shl i32 1, %7
  %14 = and i32 %12, %13
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %22, label %16

; <label>:16                                      ; preds = %9
  %17 = xor i32 %13, -1
  %18 = and i32 %12, %17
  store i32 %18, i32* %11, align 4
  %19 = bitcast i8* %data to i32*
  %20 = load i32, i32* %19, align 4
  %21 = add nsw i32 %20, -1
  store i32 %21, i32* %19, align 4
  br label %22

; <label>:22                                      ; preds = %16, %9, %5, %0
  ret void
}

attributes #0 = { norecurse nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
