	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.jump.unsigned_condition.ll"
	.text
	.globl	unsigned_condition
	.align	2
	.type	unsigned_condition,@function
	.set	nomicromips
	.set	nomips16
	.ent	unsigned_condition
unsigned_condition:                     # @unsigned_condition
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
	addiu	$2, $4, -102
	sltiu	$1, $2, 10
	bnez	$1, $BB0_3
	nop
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	lw	$4, %got(.str)($gp)
	lw	$6, %got(__FUNCTION__.unsigned_condition)($gp)
	lw	$25, %call16(fancy_abort)($gp)
	jalr	$25
	addiu	$5, $zero, 951
$BB0_3:                                 # %switch.lookup
	sll	$1, $2, 2
	lw	$2, %got(switch.table.4)($gp)
	addu	$1, $2, $1
	lw	$2, 0($1)
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	unsigned_condition
$func_end0:
	.size	unsigned_condition, ($func_end0)-unsigned_condition

	.hidden	.str
	.hidden	__FUNCTION__.unsigned_condition
	.hidden	switch.table.4

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
