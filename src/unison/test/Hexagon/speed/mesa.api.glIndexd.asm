	.text
	.file	"mesa.api.glIndexd.ll"
	.globl	glIndexd
	.align	16
	.type	glIndexd,@function
glIndexd:                               // @glIndexd
// BB#0:
	{
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			r16 = memw(##CC)
	}
	{
			call __hexagon_truncdfsf2
			r17 = memw(r16 + #380)
	}
	{
			r0 = r16
			callr r17
			r1 = r0
			r17:16 = memd(r29 + #0)
	}
	{
			dealloc_return
	}
.Lfunc_end0:
	.size	glIndexd, .Lfunc_end0-glIndexd


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
