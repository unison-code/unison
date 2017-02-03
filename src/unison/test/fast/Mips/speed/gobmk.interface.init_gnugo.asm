	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.interface.init_gnugo.ll"
	.text
	.globl	init_gnugo
	.align	2
	.type	init_gnugo,@function
	.set	nomicromips
	.set	nomips16
	.ent	init_gnugo
init_gnugo:                             # @init_gnugo
	.frame	$sp,24,$ra
	.mask 	0x80010000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addiu	$sp, $sp, -24
	sw	$ra, 20($sp)            # 4-byte Folded Spill
	sw	$16, 16($sp)            # 4-byte Folded Spill
	addu	$16, $2, $25
	lw	$25, %call16(reading_cache_init)($16)
	jalr	$25
	move	 $gp, $16
	lw	$25, %call16(clear_board)($16)
	jalr	$25
	move	 $gp, $16
	lw	$25, %call16(transformation_init)($16)
	jalr	$25
	move	 $gp, $16
	lw	$25, %call16(dfa_match_init)($16)
	jalr	$25
	move	 $gp, $16
	lw	$16, 16($sp)            # 4-byte Folded Reload
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	init_gnugo
$func_end0:
	.size	init_gnugo, ($func_end0)-init_gnugo


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
