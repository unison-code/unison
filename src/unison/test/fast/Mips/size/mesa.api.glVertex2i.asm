	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"mesa.api.glVertex2i.ll"
	.text
	.globl	glVertex2i
	.align	2
	.type	glVertex2i,@function
	.set	nomicromips
	.set	nomips16
	.ent	glVertex2i
glVertex2i:                             # @glVertex2i
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
	addu	$1, $2, $25
	lw	$1, %got(CC)($1)
	lw	$1, 0($1)
	lw	$25, 656($1)
	mtc1	$4, $f0
	cvt.s.w	$f0, $f0
	mfc1	$2, $f0
	mtc1	$5, $f0
	cvt.s.w	$f0, $f0
	mfc1	$6, $f0
	lui	$3, 16256
	sw	$3, 16($sp)
	move	 $4, $1
	move	 $5, $2
	jalr	$25
	addiu	$7, $zero, 0
	lw	$ra, 28($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 32
	.set	at
	.set	macro
	.set	reorder
	.end	glVertex2i
$func_end0:
	.size	glVertex2i, ($func_end0)-glVertex2i


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
