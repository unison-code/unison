; ModuleID = 'gobmk.patterns.autohelperpat1088.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

@transformation = external global [1369 x [8 x i32]], align 4

; Function Attrs: nounwind
define hidden i32 @autohelperpat1088(i32 %trans, i32 %move, i32 %color, i32 %action) #0 {
  %1 = getelementptr inbounds [1369 x [8 x i32]], [1369 x [8 x i32]]* @transformation, i32 0, i32 720, i32 %trans
  %2 = load i32, i32* %1, align 4
  %3 = add nsw i32 %2, %move
  %4 = getelementptr inbounds [1369 x [8 x i32]], [1369 x [8 x i32]]* @transformation, i32 0, i32 683, i32 %trans
  %5 = load i32, i32* %4, align 4
  %6 = add nsw i32 %5, %move
  %7 = tail call i32 (i32, i32, i32, ...) @play_attack_defend_n(i32 %color, i32 0, i32 1, i32 %3, i32 %6) #2
  %8 = icmp eq i32 %7, 0
  %9 = zext i1 %8 to i32
  ret i32 %9
}

declare i32 @play_attack_defend_n(i32, i32, i32, ...) #1

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
