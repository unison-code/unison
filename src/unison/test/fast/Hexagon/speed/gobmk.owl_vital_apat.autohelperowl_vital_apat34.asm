	.text
	.file	"gobmk.owl_vital_apat.autohelperowl_vital_apat34.ll"
	.hidden	autohelperowl_vital_apat34
	.globl	autohelperowl_vital_apat34
	.align	16
	.type	autohelperowl_vital_apat34,@function
autohelperowl_vital_apat34:             // @autohelperowl_vital_apat34
// BB#0:
	{
			r4 = r1
			r1:0 = combine(#0, r2)
			r3 = r0
			allocframe(#16)
	}
	{
			r3 = add(##transformation, asl(r3, #2))
			r2 = #1
			memw(r29 + #8) = r4
	}
	{
			r3 = memw(r3 + ##24256)
			memw(r29+#0) = r4
	}
	{
			call play_attack_defend2_n
			r3 = add(r3, r4)
			memw(r29+#4) = r3.new
	}
	{
			dealloc_return
	}
.Lfunc_end0:
	.size	autohelperowl_vital_apat34, .Lfunc_end0-autohelperowl_vital_apat34


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
