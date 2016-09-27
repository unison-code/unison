	.text
	.file	"sphinx3.glist.glist_tail.ll"
	.globl	glist_tail
	.align	16
	.type	glist_tail,@function
glist_tail:                             // @glist_tail
// BB#0:
	{
			p0 = cmp.eq(r0, #0); if (p0.new) jump:nt .LBB0_3
			r1 = #0
	}
// BB#1:
.LBB0_2:                                // %.preheader
                                        // =>This Inner Loop Header: Depth=1
	{
			r1 = r0
			r0 = memw(r0 + #8)
			if (!cmp.eq(r0.new, #0)) jump:t .LBB0_2
	}
	{
			jump .LBB0_3
	}
.LBB0_3:                                // %.loopexit
	{
			r0 = r1
			jumpr r31
	}
.Lfunc_end0:
	.size	glist_tail, .Lfunc_end0-glist_tail


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
