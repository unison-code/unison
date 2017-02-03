	.text
	.file	"gobmk.interface.init_gnugo.ll"
	.globl	init_gnugo
	.align	16
	.type	init_gnugo,@function
init_gnugo:                             // @init_gnugo
// BB#0:
	{
			call reading_cache_init
			allocframe(#0)
	}
	{
			call clear_board
	}
	{
			call transformation_init
	}
	{
			jump dfa_match_init
			deallocframe
	}
.Lfunc_end0:
	.size	init_gnugo, .Lfunc_end0-init_gnugo


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
