; ModuleID = 'sphinx3.glist.glist_tail.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.gnode_s.40 = type { %union.anytype_s.41, %struct.gnode_s.40* }
%union.anytype_s.41 = type { double }

; Function Attrs: norecurse nounwind readonly
define %struct.gnode_s.40* @glist_tail(%struct.gnode_s.40* readonly %g) #0 {
  %1 = icmp eq %struct.gnode_s.40* %g, null
  br i1 %1, label %.loopexit, label %.preheader.preheader

.preheader.preheader:                             ; preds = %0
  br label %.preheader

.preheader:                                       ; preds = %.preheader.preheader, %.preheader
  %gn.0 = phi %struct.gnode_s.40* [ %3, %.preheader ], [ %g, %.preheader.preheader ]
  %2 = getelementptr inbounds %struct.gnode_s.40, %struct.gnode_s.40* %gn.0, i32 0, i32 1
  %3 = load %struct.gnode_s.40*, %struct.gnode_s.40** %2, align 8
  %4 = icmp eq %struct.gnode_s.40* %3, null
  br i1 %4, label %.loopexit.loopexit, label %.preheader

.loopexit.loopexit:                               ; preds = %.preheader
  %gn.0.lcssa = phi %struct.gnode_s.40* [ %gn.0, %.preheader ]
  br label %.loopexit

.loopexit:                                        ; preds = %.loopexit.loopexit, %0
  %.0 = phi %struct.gnode_s.40* [ null, %0 ], [ %gn.0.lcssa, %.loopexit.loopexit ]
  ret %struct.gnode_s.40* %.0
}

attributes #0 = { norecurse nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
