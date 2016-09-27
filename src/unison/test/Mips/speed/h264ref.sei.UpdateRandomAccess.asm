	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"h264ref.sei.UpdateRandomAccess.ll"
	.text
	.globl	UpdateRandomAccess
	.align	2
	.type	UpdateRandomAccess,@function
	.set	nomicromips
	.set	nomips16
	.ent	UpdateRandomAccess
UpdateRandomAccess:                     # @UpdateRandomAccess
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addu	$2, $2, $25
	lw	$1, %got(img)($2)
	lw	$1, 0($1)
	lw	$1, 24($1)
	addiu	$4, $zero, 2
	bne	$1, $4, $BB0_3
	addiu	$3, $zero, 0
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	lw	$1, %got(seiRandomAccess)($2)
	addiu	$3, $zero, 1
	sh	$3, 0($1)
	sb	$zero, 2($1)
$BB0_3:
	lw	$1, %got(seiHasRandomAccess_info)($2)
	jr	$ra
	sw	$3, 0($1)
	.set	at
	.set	macro
	.set	reorder
	.end	UpdateRandomAccess
$func_end0:
	.size	UpdateRandomAccess, ($func_end0)-UpdateRandomAccess


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
