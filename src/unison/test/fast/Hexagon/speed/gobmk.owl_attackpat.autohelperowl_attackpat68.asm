	.text
	.file	"gobmk.owl_attackpat.autohelperowl_attackpat68.ll"
	.hidden	autohelperowl_attackpat68
	.globl	autohelperowl_attackpat68
	.align	16
	.type	autohelperowl_attackpat68,@function
autohelperowl_attackpat68:              // @autohelperowl_attackpat68
// BB#0:
	{
			r0 = r2
			r4 = r1
			r3 = r0
			allocframe(#16)
	}
	{
			r3 = add(##transformation, asl(r3, #2))
			r2 = #2
			r1 = #0
	}
	{
			r5 = memw(r3 + ##20672)
			r3 = memw(r3 + ##21856)
	}
	{
			r5 = add(r5, r4)
			r3 = add(r3, r4)
			memw(r29 + #8) = r4
			memw(r29 + #0) = r4
	}
	{
			call play_attack_defend2_n
			memw(r29 + #12) = r5
			memw(r29 + #4) = r3
	}
	{
			dealloc_return
	}
.Lfunc_end0:
	.size	autohelperowl_attackpat68, .Lfunc_end0-autohelperowl_attackpat68


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
