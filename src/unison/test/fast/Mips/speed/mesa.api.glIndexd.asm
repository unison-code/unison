	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"mesa.api.glIndexd.ll"
	.text
	.globl	glIndexd
	.align	2
	.type	glIndexd,@function
	.set	nomicromips
	.set	nomips16
	.ent	glIndexd
glIndexd:                               # @glIndexd
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
	addu	$1, $2, $25
	lw	$1, %got(CC)($1)
	lw	$4, 0($1)
	lw	$25, 380($4)
	cvt.s.d	$f0, $f12
	jalr	$25
	mfc1	$5, $f0
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	glIndexd
$func_end0:
	.size	glIndexd, ($func_end0)-glIndexd


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
