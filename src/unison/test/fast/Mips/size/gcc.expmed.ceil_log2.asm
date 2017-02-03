	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.expmed.ceil_log2.ll"
	.text
	.globl	ceil_log2
	.align	2
	.type	ceil_log2,@function
	.set	nomicromips
	.set	nomips16
	.ent	ceil_log2
ceil_log2:                              # @ceil_log2
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
	lw	$25, %call16(floor_log2_wide)($gp)
	jalr	$25
	addiu	$4, $4, -1
	addiu	$2, $2, 1
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	ceil_log2
$func_end0:
	.size	ceil_log2, ($func_end0)-ceil_log2


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
