	.text
	.file	"hmmer.tophits.AllocFancyAli.ll"
	.globl	AllocFancyAli
	.align	16
	.type	AllocFancyAli,@function
AllocFancyAli:                          // @AllocFancyAli
// BB#0:
	{
			r2 = #40
			r0 = ##.str
			allocframe(#0)
	}
	{
			call sre_malloc
			r1 = #117
	}
	{
			memw(r0 + #8)=#0
			memw(r0 + #4)=#0
	}
	{
			memw(r0 + #28)=#0
			memw(r0 + #32)=#0
	}
	{
			memw(r0 + #36)=#0
			memw(r0 + #0)=#0
	}
	{
			memw(r0 + #12)=#0
			memw(r0 + #16)=#0
	}
	{
			memw(r0+#24)=#0
	}
	{
			dealloc_return
	}
.Lfunc_end0:
	.size	AllocFancyAli, .Lfunc_end0-AllocFancyAli

	.hidden	.str

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
