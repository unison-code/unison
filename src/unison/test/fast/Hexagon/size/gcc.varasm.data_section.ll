; ModuleID = 'gcc.varasm.data_section.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct._IO_FILE.171 = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker.172*, %struct._IO_FILE.171*, i32, i32, i32, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i32, i32, [40 x i8] }
%struct._IO_marker.172 = type { %struct._IO_marker.172*, %struct._IO_FILE.171*, i32 }

@in_section = external hidden unnamed_addr global i8, align 1
@asm_out_file = external global %struct._IO_FILE.171*, align 4
@.str = external hidden unnamed_addr constant [4 x i8], align 1
@.str.3 = external hidden unnamed_addr constant [7 x i8], align 1

; Function Attrs: nounwind
declare i32 @fprintf(%struct._IO_FILE.171* nocapture, i8* nocapture readonly, ...) #0

; Function Attrs: nounwind
define void @data_section() #0 {
  %1 = load i8, i8* @in_section, align 1
  %2 = icmp eq i8 %1, 2
  br i1 %2, label %6, label %3

; <label>:3                                       ; preds = %0
  %4 = load %struct._IO_FILE.171*, %struct._IO_FILE.171** @asm_out_file, align 4
  %5 = tail call i32 (%struct._IO_FILE.171*, i8*, ...) @fprintf(%struct._IO_FILE.171* %4, i8* nonnull getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i8* nonnull getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i32 0, i32 0))
  store i8 2, i8* @in_section, align 1
  br label %6

; <label>:6                                       ; preds = %3, %0
  ret void
}

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
