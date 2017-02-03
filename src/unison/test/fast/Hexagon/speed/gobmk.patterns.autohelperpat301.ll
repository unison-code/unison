; ModuleID = 'gobmk.patterns.autohelperpat301.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct.influence_data.965 = type opaque

@transformation = external global [1369 x [8 x i32]], align 8
@initial_white_influence = external global %struct.influence_data.965, align 1
@initial_black_influence = external global %struct.influence_data.965, align 1

; Function Attrs: nounwind
define hidden i32 @autohelperpat301(i32 %trans, i32 %move, i32 %color, i32 %action) #0 {
  %1 = getelementptr inbounds [1369 x [8 x i32]], [1369 x [8 x i32]]* @transformation, i32 0, i32 795, i32 %trans
  %2 = load i32, i32* %1, align 4
  %3 = add nsw i32 %2, %move
  %4 = icmp eq i32 %color, 2
  %5 = select i1 %4, %struct.influence_data.965* @initial_white_influence, %struct.influence_data.965* @initial_black_influence
  %6 = tail call i32 @whose_moyo(%struct.influence_data.965* %5, i32 %3) #2
  %7 = icmp eq i32 %6, %color
  %8 = zext i1 %7 to i32
  ret i32 %8
}

declare i32 @whose_moyo(%struct.influence_data.965*, i32) #1

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv60" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
