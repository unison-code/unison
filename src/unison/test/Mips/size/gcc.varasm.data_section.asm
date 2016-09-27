	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.varasm.data_section.ll"
	.text
	.globl	data_section
	.align	2
	.type	data_section,@function
	.set	nomicromips
	.set	nomips16
	.ent	data_section
data_section:                           # @data_section
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
	addu	$gp, $2, $25
	lw	$16, %got(in_section)($gp)
	lw	$1, 0($16)
	addiu	$17, $zero, 2
	beq	$1, $17, $BB0_3
	nop
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	lw	$1, %got(asm_out_file)($gp)
	lw	$5, %got(.str)($gp)
	lw	$6, %got(.str.3)($gp)
	lw	$25, %call16(fprintf)($gp)
	jalr	$25
	lw	$4, 0($1)
	sw	$17, 0($16)
$BB0_3:
	lw	$16, 20($sp)            # 4-byte Folded Reload
	lw	$17, 24($sp)            # 4-byte Folded Reload
	lw	$ra, 28($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 32
	.set	at
	.set	macro
	.set	reorder
	.end	data_section
$func_end0:
	.size	data_section, ($func_end0)-data_section

	.hidden	in_section
	.hidden	.str
	.hidden	.str.3

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
