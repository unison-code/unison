	.text
	.file	"gcc.expmed.ceil_log2.ll"
	.globl	ceil_log2
	.align	16
	.type	ceil_log2,@function
ceil_log2:                              // @ceil_log2
// BB#0:
	{
			call floor_log2_wide
			r0 = add(r0,#-1)
			allocframe(#0)
	}
	{
			r0 = add(r0, #1)
			dealloc_return
	}
.Lfunc_end0:
	.size	ceil_log2, .Lfunc_end0-ceil_log2


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
