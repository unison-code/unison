	.text
	.file	"gcc.insn-output.output_51.ll"
	.hidden	output_51
	.globl	output_51
	.align	16
	.type	output_51,@function
output_51:                              // @output_51
// BB#0:
	{
			r16 = r1
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			call get_attr_type
			r0 = r16
	}
	{
			r0 = ##.str.2014
			r1 = r0
	}
	{
			p0 = cmp.eq(r1, #8); if (p0.new) jump:nt .LBB0_2
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			call get_attr_mode
			r0 = r16
	}
	{
			p0 = cmp.eq(r0, #4)
			if (p0.new) r0 = ##.str.2015
	}
	{
			if (!p0) r0 = ##.str.75
	}
.LBB0_2:
	{
			r17:16 = memd(r29 + #0)
			dealloc_return
	}
.Lfunc_end0:
	.size	output_51, .Lfunc_end0-output_51

	.hidden	.str.75
	.hidden	.str.2014
	.hidden	.str.2015

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
