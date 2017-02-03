; ModuleID = 'gobmk.board.get_last_player.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

@move_history_pointer = external global i32, align 4
@move_history_color = external global [500 x i32], align 4

; Function Attrs: norecurse nounwind readonly
define i32 @get_last_player() #0 {
  %1 = load i32, i32* @move_history_pointer, align 4
  %2 = icmp eq i32 %1, 0
  br i1 %2, label %7, label %3

; <label>:3                                       ; preds = %0
  %4 = add nsw i32 %1, -1
  %5 = getelementptr inbounds [500 x i32], [500 x i32]* @move_history_color, i32 0, i32 %4
  %6 = load i32, i32* %5, align 4
  br label %7

; <label>:7                                       ; preds = %3, %0
  %.0 = phi i32 [ %6, %3 ], [ 0, %0 ]
  ret i32 %.0
}

attributes #0 = { norecurse nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
