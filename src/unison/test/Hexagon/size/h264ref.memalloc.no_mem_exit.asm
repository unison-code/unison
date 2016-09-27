	.text
	.file	"h264ref.memalloc.no_mem_exit.ll"
	.globl	no_mem_exit
	.align	16
	.type	no_mem_exit,@function
no_mem_exit:                            // @no_mem_exit
// BB#0:
	{
			r2 = ##.str.20
			r3 = r0
			allocframe(#8)
	}
	{
			r1 = #300
			r0 = ##errortext
			memw(r29 + #0) = r3
	}
	{
			call snprintf
	}
	{
			r1:0 = combine(#100, ##errortext)
			deallocframe
	}
	{
			jump error
	}
.Lfunc_end0:
	.size	no_mem_exit, .Lfunc_end0-no_mem_exit

	.hidden	.str.20

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
