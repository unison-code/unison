	.text
	.file	"gcc.alias.get_frame_alias_set.ll"
	.globl	get_frame_alias_set
	.align	16
	.type	get_frame_alias_set,@function
get_frame_alias_set:                    // @get_frame_alias_set
// BB#0:
	{
			r0 = memw(##get_frame_alias_set.set)
			if (!cmp.eq(r0.new, #-1)) jump:t .LBB0_4
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			r0 = #0
			r1 = memw(##flag_strict_aliasing)
			if (cmp.eq(r1.new, #0)) jump:nt .LBB0_3
	}
	{
			jump .LBB0_2
	}
.LBB0_2:
	{
			r0 = memw(##new_alias_set.last_alias_set)
	}
	{
			r0 = add(r0, #1)
			memw(###new_alias_set.last_alias_set) = r0.new
	}
.LBB0_3:                                // %new_alias_set.exit
	{
			memw(##get_frame_alias_set.set) = r0
	}
.LBB0_4:
	{
			jumpr r31
	}
.Lfunc_end0:
	.size	get_frame_alias_set, .Lfunc_end0-get_frame_alias_set

	.hidden	new_alias_set.last_alias_set
	.hidden	get_frame_alias_set.set

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
