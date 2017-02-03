	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.rtlanal.insn_dependent_p_1.ll"
	.text
	.hidden	insn_dependent_p_1
	.globl	insn_dependent_p_1
	.align	2
	.type	insn_dependent_p_1,@function
	.set	nomicromips
	.set	nomips16
	.ent	insn_dependent_p_1
insn_dependent_p_1:                     # @insn_dependent_p_1
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
	move	 $16, $6
	lw	$5, 0($16)
	beqz	$5, $BB0_5
	addu	$gp, $2, $25
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	lw	$25, %call16(reg_mentioned_p)($gp)
	jalr	$25
	nop
	beqz	$2, $BB0_5
	nop
# BB#3:
	b	$BB0_4
	nop
$BB0_4:
	sw	$zero, 0($16)
$BB0_5:
	lw	$16, 16($sp)            # 4-byte Folded Reload
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	insn_dependent_p_1
$func_end0:
	.size	insn_dependent_p_1, ($func_end0)-insn_dependent_p_1


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
