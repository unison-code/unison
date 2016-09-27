	.text
	.file	"mesa.api.glVertex2i.ll"
	.globl	glVertex2i
	.align	16
	.type	glVertex2i,@function
glVertex2i:                             // @glVertex2i
// BB#0:
	{
			memd(r29 + #-16) = r17:16
			allocframe(#16)
	}
	{
			r17 = r1
			r16 = memw(##CC)
			memd(r29+#0) = r19:18
	}
	{
			call __hexagon_floatsisf
			r19 = memw(r16 + #656)
	}
	{
			call __hexagon_floatsisf
			r18 = r0
			r0 = r17
	}
	{
			r1:0 = combine(r18, r16)
			r4 = ##1065353216
			r3:2 = combine(#0, r0)
	}
	{
			callr r19
			r17:16 = memd(r29 + #8)
			r19:18 = memd(r29 + #0)
	}
	{
			dealloc_return
	}
.Lfunc_end0:
	.size	glVertex2i, .Lfunc_end0-glVertex2i


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
