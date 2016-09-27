; ModuleID = 'gcc.alias.get_frame_alias_set.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

@flag_strict_aliasing = external global i32, align 4
@new_alias_set.last_alias_set = external hidden unnamed_addr global i32, align 4
@get_frame_alias_set.set = external hidden unnamed_addr global i32, align 4

; Function Attrs: norecurse nounwind
define i32 @get_frame_alias_set() #0 {
  %1 = load i32, i32* @get_frame_alias_set.set, align 4
  %2 = icmp eq i32 %1, -1
  br i1 %2, label %3, label %9

; <label>:3                                       ; preds = %0
  %4 = load i32, i32* @flag_strict_aliasing, align 4
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %new_alias_set.exit, label %6

; <label>:6                                       ; preds = %3
  %7 = load i32, i32* @new_alias_set.last_alias_set, align 4
  %8 = add nsw i32 %7, 1
  store i32 %8, i32* @new_alias_set.last_alias_set, align 4
  br label %new_alias_set.exit

new_alias_set.exit:                               ; preds = %6, %3
  %.0.i = phi i32 [ %8, %6 ], [ 0, %3 ]
  store i32 %.0.i, i32* @get_frame_alias_set.set, align 4
  br label %9

; <label>:9                                       ; preds = %new_alias_set.exit, %0
  %10 = phi i32 [ %.0.i, %new_alias_set.exit ], [ %1, %0 ]
  ret i32 %10
}

attributes #0 = { norecurse nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
