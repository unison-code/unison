; ModuleID = 'gobmk.board.get_last_player.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

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

attributes #0 = { norecurse nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
