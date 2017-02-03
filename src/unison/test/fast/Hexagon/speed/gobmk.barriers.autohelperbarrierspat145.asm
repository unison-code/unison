	.text
	.file	"gobmk.barriers.autohelperbarrierspat145.ll"
	.hidden	autohelperbarrierspat145
	.globl	autohelperbarrierspat145
	.align	16
	.type	autohelperbarrierspat145,@function
autohelperbarrierspat145:               // @autohelperbarrierspat145
// BB#0:
	{
			r3 = r1
			r16 = r2
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			r0 = add(##transformation, asl(r0, #2))
			r1 = r16
	}
	{
			r2 = memw(r0 + ##19552)
			r4 = memw(r0 + ##20736)
	}
	{
			call influence_mark_non_territory
			r0 = add(r2, r3)
			r17 = add(r4, r3)
	}
	{
			call influence_mark_non_territory
			r1:0 = combine(r16, r17)
	}
	{
			r0 = #0
			r17:16 = memd(r29 + #0)
			dealloc_return
	}
.Lfunc_end0:
	.size	autohelperbarrierspat145, .Lfunc_end0-autohelperbarrierspat145


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
