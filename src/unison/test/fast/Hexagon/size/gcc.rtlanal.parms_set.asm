	.text
	.file	"gcc.rtlanal.parms_set.ll"
	.hidden	parms_set
	.globl	parms_set
	.align	16
	.type	parms_set,@function
parms_set:                              // @parms_set
// BB#0:
	{
			r1 = memub(r0 + #0)
	}
	{
			if (!p0.new) jump:nt .LBB0_4
			p0 = cmp.eq(r1, #61)
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			r1 = memw(r0 + #4)
	}
	{
			if (p0.new) jump:nt .LBB0_4
			p0 = cmp.gtu(r1, #52)
	}
	{
			jump .LBB0_2
	}
.LBB0_2:
	{
			r0 = memw(r2 + #4)
	}
	{
			p0 = !tstbit(r0, r1)
			if (p0.new) jump:nt .LBB0_4
	}
	{
			jump .LBB0_3
	}
.LBB0_3:
	{
			r1 = lsl(#1, r1)
			r3 = #-1
			memw(r2+#0) -= #1
	}
	{
			r0 &= xor(r1, r3)
			memw(r2+#4) = r0.new
	}
.LBB0_4:
	{
			jumpr r31
	}
.Lfunc_end0:
	.size	parms_set, .Lfunc_end0-parms_set


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
