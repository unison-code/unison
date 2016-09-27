; ModuleID = 'gcc.c-decl.pushdecl_top_level.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

%struct.binding_level.2606 = type { %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %struct.binding_level.2606*, i8, i8, i8, i8, i8, i32, %union.tree_node.2601* }
%union.tree_node.2601 = type { %struct.tree_type.2602, [24 x i8] }
%struct.tree_type.2602 = type { %struct.tree_common.2603, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, i32, i32, i32, %union.tree_node.2601*, %union.tree_node.2601*, %union.anon.2.2604, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, %union.tree_node.2601*, i32, %struct.lang_type.2605* }
%struct.tree_common.2603 = type { %union.tree_node.2601*, %union.tree_node.2601*, i32 }
%union.anon.2.2604 = type { i32 }
%struct.lang_type.2605 = type { i32, [1 x %union.tree_node.2601*] }

@current_binding_level = external hidden global %struct.binding_level.2606*, align 4
@global_binding_level = external hidden unnamed_addr global %struct.binding_level.2606*, align 4

; Function Attrs: nounwind
declare %union.tree_node.2601* @pushdecl(%union.tree_node.2601*) #0

; Function Attrs: nounwind
define %union.tree_node.2601* @pushdecl_top_level(%union.tree_node.2601* %x) #0 {
  %1 = load i32, i32* bitcast (%struct.binding_level.2606** @current_binding_level to i32*), align 4
  %2 = load i32, i32* bitcast (%struct.binding_level.2606** @global_binding_level to i32*), align 4
  store i32 %2, i32* bitcast (%struct.binding_level.2606** @current_binding_level to i32*), align 4
  %3 = tail call %union.tree_node.2601* @pushdecl(%union.tree_node.2601* %x)
  store i32 %1, i32* bitcast (%struct.binding_level.2606** @current_binding_level to i32*), align 4
  ret %union.tree_node.2601* %3
}

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
