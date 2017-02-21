	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.insn-output.output_51.ll"
	.text
	.hidden	output_51
	.globl	output_51
	.align	2
	.type	output_51,@function
	.set	nomicromips
	.set	nomips16
	.ent	output_51
output_51:                              # @output_51
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
	move	 $17, $5
	lw	$25, %call16(get_attr_type)($16)
	move	 $4, $17
	jalr	$25
	move	 $gp, $16
	addiu	$1, $zero, 8
	bne	$2, $1, $BB0_2
	nop
# BB#1:
	lw	$2, %got(.str.2014)($16)
	b	$BB0_3
	nop
$BB0_2:
	lw	$25, %call16(get_attr_mode)($16)
	move	 $4, $17
	jalr	$25
	move	 $gp, $16
	xori	$1, $2, 4
	addiu	$2, $16, %got(.str.75)
	addiu	$3, $16, %got(.str.2015)
	movz	$2, $3, $1
	lw	$2, 0($2)
$BB0_3:
	lw	$16, 20($sp)            # 4-byte Folded Reload
	lw	$17, 24($sp)            # 4-byte Folded Reload
	lw	$ra, 28($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 32
	.set	at
	.set	macro
	.set	reorder
	.end	output_51
$func_end0:
	.size	output_51, ($func_end0)-output_51

	.hidden	.str.75
	.hidden	.str.2014
	.hidden	.str.2015

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
