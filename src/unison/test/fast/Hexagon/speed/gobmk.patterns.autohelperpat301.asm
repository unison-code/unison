	.text
	.file	"gobmk.patterns.autohelperpat301.ll"
	.hidden	autohelperpat301
	.globl	autohelperpat301
	.align	16
	.type	autohelperpat301,@function
autohelperpat301:                       // @autohelperpat301
// BB#0:
	{
			r16 = r2
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			r0 = add(##transformation, asl(r0, #2))
			p0 = cmp.eq(r16, #2)
	}
	{
			if (p0) r2 = ##initial_white_influence
			r3 = memw(r0 + ##25440)
	}
	{
			if (!p0) r2 = ##initial_black_influence
	}
	{
			call whose_moyo
			r0 = r2
			r1 = add(r3, r1)
	}
	{
			r0 = cmp.eq(r0, r16)
			r17:16 = memd(r29 + #0)
			dealloc_return
	}
.Lfunc_end0:
	.size	autohelperpat301, .Lfunc_end0-autohelperpat301


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
