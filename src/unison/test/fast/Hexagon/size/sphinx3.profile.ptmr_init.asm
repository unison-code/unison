	.text
	.file	"sphinx3.profile.ptmr_init.ll"
	.globl	ptmr_init
	.align	16
	.type	ptmr_init,@function
ptmr_init:                              // @ptmr_init
// BB#0:
	{
			memw(r0 + #12)=#0
			memw(r0 + #16)=#0
	}
	{
			memw(r0 + #20)=#0
			memw(r0 + #24)=#0
	}
	{
			memw(r0 + #28)=#0
			memw(r0 + #32)=#0
	}
	{
			jumpr r31
			memw(r0 + #36)=#0
			memw(r0 + #8)=#0
	}
.Lfunc_end0:
	.size	ptmr_init, .Lfunc_end0-ptmr_init


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
