	.text
	.file	"gcc.rtlanal.insn_dependent_p_1.ll"
	.hidden	insn_dependent_p_1
	.globl	insn_dependent_p_1
	.align	16
	.type	insn_dependent_p_1,@function
insn_dependent_p_1:                     // @insn_dependent_p_1
// BB#0:
	{
			r16 = r2
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			r1 = memw(r16 + #0)
			if (cmp.eq(r1.new, #0)) jump:nt .LBB0_3
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			call reg_mentioned_p
	}
	{
			p0 = cmp.eq(r0, #0); if (p0.new) jump:nt .LBB0_3
	}
	{
			jump .LBB0_2
	}
.LBB0_2:
	{
			memw(r16+#0)=#0
	}
.LBB0_3:
	{
			r17:16 = memd(r29 + #0)
			dealloc_return
	}
.Lfunc_end0:
	.size	insn_dependent_p_1, .Lfunc_end0-insn_dependent_p_1


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
