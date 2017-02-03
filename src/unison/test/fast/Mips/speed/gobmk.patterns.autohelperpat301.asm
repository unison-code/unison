	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.patterns.autohelperpat301.ll"
	.text
	.hidden	autohelperpat301
	.globl	autohelperpat301
	.align	2
	.type	autohelperpat301,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperpat301
autohelperpat301:                       # @autohelperpat301
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
	addu	$gp, $2, $25
	move	 $16, $6
	xori	$1, $16, 2
	addiu	$2, $gp, %got(initial_black_influence)
	addiu	$3, $gp, %got(initial_white_influence)
	movz	$2, $3, $1
	sll	$1, $4, 2
	lw	$3, %got(transformation)($gp)
	lw	$4, 0($2)
	addu	$1, $3, $1
	lw	$1, 25440($1)
	lw	$25, %call16(whose_moyo)($gp)
	jalr	$25
	addu	$5, $1, $5
	xor	$1, $2, $16
	sltiu	$2, $1, 1
	lw	$16, 16($sp)            # 4-byte Folded Reload
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperpat301
$func_end0:
	.size	autohelperpat301, ($func_end0)-autohelperpat301


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
