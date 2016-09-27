	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.board.get_last_player.ll"
	.text
	.globl	get_last_player
	.align	2
	.type	get_last_player,@function
	.set	nomicromips
	.set	nomips16
	.ent	get_last_player
get_last_player:                        # @get_last_player
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addu	$3, $2, $25
	lw	$1, %got(move_history_pointer)($3)
	lw	$4, 0($1)
	beqz	$4, $BB0_3
	addiu	$2, $zero, 0
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	sll	$1, $4, 2
	lw	$2, %got(move_history_color)($3)
	addu	$1, $1, $2
	lw	$2, -4($1)
$BB0_3:
	jr	$ra
	nop
	.set	at
	.set	macro
	.set	reorder
	.end	get_last_player
$func_end0:
	.size	get_last_player, ($func_end0)-get_last_player


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
