	.text
	.file	"gcc.xexit.xexit.ll"
	.globl	xexit
	.align	16
	.type	xexit,@function
xexit:                                  // @xexit
// BB#0:
	{
			r16 = r0
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			r1 = memw(##_xexit_cleanup)
			if (cmp.eq(r1.new, #0)) jump:t .LBB0_2
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			callr r1
	}
.LBB0_2:
	{
			call exit
			r0 = r16
	}
	{
			r17:16 = memd(r29 + #0)
			deallocframe
	}
.Lfunc_end0:
	.size	xexit, .Lfunc_end0-xexit


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
