	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.patterns.autohelperpat1088.ll"
	.text
	.hidden	autohelperpat1088
	.globl	autohelperpat1088
	.align	2
	.type	autohelperpat1088,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperpat1088
autohelperpat1088:                      # @autohelperpat1088
	.frame	$sp,32,$ra
	.mask 	0x80000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addiu	$sp, $sp, -32
	sw	$ra, 28($sp)            # 4-byte Folded Spill
	addu	$gp, $2, $25
	sll	$1, $4, 2
	lw	$2, %got(transformation)($gp)
	addu	$1, $2, $1
	lw	$2, 23040($1)
	lw	$1, 21856($1)
	addu	$1, $1, $5
	sw	$1, 16($sp)
	addu	$7, $2, $5
	lw	$25, %call16(play_attack_defend_n)($gp)
	move	 $4, $6
	addiu	$5, $zero, 0
	jalr	$25
	addiu	$6, $zero, 1
	sltiu	$2, $2, 1
	lw	$ra, 28($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 32
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperpat1088
$func_end0:
	.size	autohelperpat1088, ($func_end0)-autohelperpat1088


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
