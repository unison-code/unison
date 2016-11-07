; ModuleID = 'factorial.ll'
target datalayout = "e-m:e-p:32:32-i1:32-i64:64-a:0-n32"
target triple = "hexagon"

; Function Attrs: norecurse nounwind readnone
define i32 @factorial(i32 %n) #0 {
entry:
  %cmp4 = icmp sgt i32 %n, 0
  br i1 %cmp4, label %while.body.preheader, label %while.end

while.body.preheader:                             ; preds = %entry
  br label %while.body

while.body:                                       ; preds = %while.body.preheader, %while.body
  %f.06 = phi i32 [ %mul, %while.body ], [ 1, %while.body.preheader ]
  %n.addr.05 = phi i32 [ %dec, %while.body ], [ %n, %while.body.preheader ]
  %dec = add nsw i32 %n.addr.05, -1
  %mul = mul nsw i32 %n.addr.05, %f.06
  %cmp = icmp sgt i32 %n.addr.05, 1
  br i1 %cmp, label %while.body, label %while.end.loopexit

while.end.loopexit:                               ; preds = %while.body
  %mul.lcssa = phi i32 [ %mul, %while.body ]
  br label %while.end

while.end:                                        ; preds = %while.end.loopexit, %entry
  %f.0.lcssa = phi i32 [ 1, %entry ], [ %mul.lcssa, %while.end.loopexit ]
  ret i32 %f.0.lcssa
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv4" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0"}
