; ModuleID = 'gcc.insn-output.output_51.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

%struct.rtx_def.183 = type { i32, [1 x %union.rtunion_def.185] }
%union.rtunion_def.185 = type { i32 }

@.str.75 = external hidden unnamed_addr constant [23 x i8], align 1
@.str.2014 = external hidden unnamed_addr constant [29 x i8], align 1
@.str.2015 = external hidden unnamed_addr constant [27 x i8], align 1

; Function Attrs: nounwind
define hidden i8* @output_51(%struct.rtx_def.183** nocapture readnone %operands, %struct.rtx_def.183* %insn) #0 {
  %1 = tail call i32 @get_attr_type(%struct.rtx_def.183* %insn) #2
  %cond = icmp eq i32 %1, 8
  br i1 %cond, label %5, label %2

; <label>:2                                       ; preds = %0
  %3 = tail call i32 @get_attr_mode(%struct.rtx_def.183* %insn) #2
  %4 = icmp eq i32 %3, 4
  %. = select i1 %4, i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str.2015, i32 0, i32 0), i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.75, i32 0, i32 0)
  br label %5

; <label>:5                                       ; preds = %2, %0
  %.0 = phi i8* [ getelementptr inbounds ([29 x i8], [29 x i8]* @.str.2014, i32 0, i32 0), %0 ], [ %., %2 ]
  ret i8* %.0
}

declare i32 @get_attr_type(%struct.rtx_def.183*) #1

declare i32 @get_attr_mode(%struct.rtx_def.183*) #1

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
