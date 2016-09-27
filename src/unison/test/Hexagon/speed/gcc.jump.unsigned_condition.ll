; ModuleID = 'gcc.jump.unsigned_condition.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

@.str = external hidden unnamed_addr constant [7 x i8], align 1
@__FUNCTION__.unsigned_condition = external hidden unnamed_addr constant [19 x i8], align 1

; Function Attrs: noreturn
declare void @fancy_abort(i8*, i32, i8*) #0

; Function Attrs: nounwind
define zeroext i8 @unsigned_condition(i8 zeroext %code) #1 {
  %1 = zext i8 %code to i32
  switch i32 %1, label %6 [
    i32 103, label %7
    i32 102, label %7
    i32 109, label %7
    i32 108, label %7
    i32 111, label %7
    i32 110, label %7
    i32 105, label %2
    i32 104, label %3
    i32 107, label %4
    i32 106, label %5
  ]

; <label>:2                                       ; preds = %0
  br label %7

; <label>:3                                       ; preds = %0
  br label %7

; <label>:4                                       ; preds = %0
  br label %7

; <label>:5                                       ; preds = %0
  br label %7

; <label>:6                                       ; preds = %0
  tail call void @fancy_abort(i8* nonnull getelementptr inbounds ([7 x i8], [7 x i8]* @.str, i32 0, i32 0), i32 951, i8* nonnull getelementptr inbounds ([19 x i8], [19 x i8]* @__FUNCTION__.unsigned_condition, i32 0, i32 0)) #2
  unreachable

; <label>:7                                       ; preds = %5, %4, %3, %2, %0, %0, %0, %0, %0, %0
  %.0 = phi i8 [ 110, %5 ], [ 111, %4 ], [ 108, %3 ], [ 109, %2 ], [ %code, %0 ], [ %code, %0 ], [ %code, %0 ], [ %code, %0 ], [ %code, %0 ], [ %code, %0 ]
  ret i8 %.0
}

attributes #0 = { noreturn "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
