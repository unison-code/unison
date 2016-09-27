	.text
	.file	"gobmk.owl_defendpat.autohelperowl_defendpat421.ll"
	.hidden	autohelperowl_defendpat421
	.globl	autohelperowl_defendpat421
	.align	16
	.type	autohelperowl_defendpat421,@function
autohelperowl_defendpat421:             // @autohelperowl_defendpat421
// BB#0:
	{
			r1 = sub(#3, r2)
			r3 = r1
			allocframe(#0)
	}
	{
			r0 = add(##transformation, asl(r0, #2))
	}
	{
			r0 = memw(r0 + ##18336)
	}
	{
			call safe_move
			r0 = add(r0, r3)
	}
	{
			r0 = cmp.eq(r0, #0)
			dealloc_return
	}
.Lfunc_end0:
	.size	autohelperowl_defendpat421, .Lfunc_end0-autohelperowl_defendpat421


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
