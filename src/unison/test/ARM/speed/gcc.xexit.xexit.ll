; ModuleID = 'gcc.xexit.xexit.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

@_xexit_cleanup = external global void ()*, align 4

; Function Attrs: noreturn nounwind
define void @xexit(i32 %code) #0 {
  %1 = load void ()*, void ()** @_xexit_cleanup, align 4
  %2 = icmp eq void ()* %1, null
  br i1 %2, label %4, label %3

; <label>:3                                       ; preds = %0
  tail call void %1() #1
  br label %4

; <label>:4                                       ; preds = %3, %0
  tail call void @exit(i32 %code) #2
  unreachable
}

; Function Attrs: noreturn nounwind
declare void @exit(i32) #0

attributes #0 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }
attributes #2 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
