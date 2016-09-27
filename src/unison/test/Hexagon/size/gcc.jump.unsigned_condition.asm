	.text
	.file	"gcc.jump.unsigned_condition.ll"
	.globl	unsigned_condition
	.align	16
	.type	unsigned_condition,@function
unsigned_condition:                     // @unsigned_condition
// BB#0:
	{
			r1 = add(r0, #-102)
			if (cmp.gtu(r1.new, #9)) jump:nt .LBB0_6
	}
// BB#1:
	{
			r2 = ##.LJTI0_0
	}
	{
			r1 = memw(r2 + r1<<#2)
	}
	{
			jumpr r1
	}
.LBB0_2:
	{
			jump .LBB0_7
			r0 = #109
	}
.LBB0_3:
	{
			jump .LBB0_7
			r0 = #108
	}
.LBB0_4:
	{
			jump .LBB0_7
			r0 = #111
	}
.LBB0_5:
	{
			jump .LBB0_7
			r0 = #110
	}
.LBB0_6:
	{
			r0 = ##.str
			allocframe(#0)
	}
	{
			call fancy_abort
			r2 = ##__FUNCTION__.unsigned_condition
			r1 = #951
	}
	{
			deallocframe
	}
.LBB0_7:
	{
			jumpr r31
	}
.Lfunc_end0:
	.size	unsigned_condition, .Lfunc_end0-unsigned_condition
	.section	.rodata,"a",@progbits
	.align	4
.LJTI0_0:
	.word	.LBB0_7
	.word	.LBB0_7
	.word	.LBB0_3
	.word	.LBB0_2
	.word	.LBB0_5
	.word	.LBB0_4
	.word	.LBB0_7
	.word	.LBB0_7
	.word	.LBB0_7
	.word	.LBB0_7

	.hidden	.str
	.hidden	__FUNCTION__.unsigned_condition

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
