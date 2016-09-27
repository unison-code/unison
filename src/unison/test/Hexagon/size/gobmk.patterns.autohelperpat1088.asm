	.text
	.file	"gobmk.patterns.autohelperpat1088.ll"
	.hidden	autohelperpat1088
	.globl	autohelperpat1088
	.align	16
	.type	autohelperpat1088,@function
autohelperpat1088:                      // @autohelperpat1088
// BB#0:
	{
			r4 = r1
			r1:0 = combine(#0, r2)
			r3 = r0
			allocframe(#8)
	}
	{
			r3 = add(##transformation, asl(r3, #2))
			r2 = #1
	}
	{
			r5 = memw(r3 + ##21856)
			r3 = memw(r3 + ##23040)
	}
	{
			r5 = add(r5, r4)
			r3 = add(r3, r4)
			memw(r29+#4) = r5.new
	}
	{
			call play_attack_defend_n
			memw(r29+#0) = r3
	}
	{
			r0 = cmp.eq(r0, #0)
			dealloc_return
	}
.Lfunc_end0:
	.size	autohelperpat1088, .Lfunc_end0-autohelperpat1088


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
