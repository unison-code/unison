	.text
	.file	"gobmk.board.get_last_player.ll"
	.globl	get_last_player
	.align	16
	.type	get_last_player,@function
get_last_player:                        // @get_last_player
// BB#0:
	{
			r0 = #0
			r1 = memw(##move_history_pointer)
			if (cmp.eq(r1.new, #0)) jump:nt .LBB0_2
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			r1 = add(##move_history_color, asl(r1, #2))
	}
	{
			r0 = memw(r1 + #-4)
	}
.LBB0_2:
	{
			jumpr r31
	}
.Lfunc_end0:
	.size	get_last_player, .Lfunc_end0-get_last_player


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
