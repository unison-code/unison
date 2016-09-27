	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.xexit.xexit.ll"
	.text
	.globl	xexit
	.align	2
	.type	xexit,@function
	.set	nomicromips
	.set	nomips16
	.ent	xexit
xexit:                                  # @xexit
	.frame	$sp,32,$ra
	.mask 	0x80030000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addiu	$sp, $sp, -32
	sw	$ra, 28($sp)            # 4-byte Folded Spill
	sw	$17, 24($sp)            # 4-byte Folded Spill
	sw	$16, 20($sp)            # 4-byte Folded Spill
	addu	$16, $2, $25
	lw	$1, %got(_xexit_cleanup)($16)
	lw	$25, 0($1)
	beqz	$25, $BB0_3
	move	 $17, $4
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	jalr	$25
	nop
$BB0_3:
	lw	$25, %call16(exit)($16)
	move	 $4, $17
	jalr	$25
	move	 $gp, $16
	.set	at
	.set	macro
	.set	reorder
	.end	xexit
$func_end0:
	.size	xexit, ($func_end0)-xexit


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
