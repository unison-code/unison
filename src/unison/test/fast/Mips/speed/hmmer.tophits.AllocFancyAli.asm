	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"hmmer.tophits.AllocFancyAli.ll"
	.text
	.globl	AllocFancyAli
	.align	2
	.type	AllocFancyAli,@function
	.set	nomicromips
	.set	nomips16
	.ent	AllocFancyAli
AllocFancyAli:                          # @AllocFancyAli
	.frame	$sp,24,$ra
	.mask 	0x80000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addiu	$sp, $sp, -24
	sw	$ra, 20($sp)            # 4-byte Folded Spill
	addu	$gp, $2, $25
	lw	$4, %got(.str)($gp)
	lw	$25, %call16(sre_malloc)($gp)
	addiu	$5, $zero, 117
	jalr	$25
	addiu	$6, $zero, 40
	sw	$zero, 16($2)
	sw	$zero, 12($2)
	sw	$zero, 8($2)
	sw	$zero, 4($2)
	sw	$zero, 0($2)
	sw	$zero, 36($2)
	sw	$zero, 32($2)
	sw	$zero, 28($2)
	sw	$zero, 24($2)
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	AllocFancyAli
$func_end0:
	.size	AllocFancyAli, ($func_end0)-AllocFancyAli

	.hidden	.str

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
