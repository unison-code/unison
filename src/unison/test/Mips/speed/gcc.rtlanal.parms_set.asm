	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.rtlanal.parms_set.ll"
	.text
	.hidden	parms_set
	.globl	parms_set
	.align	2
	.type	parms_set,@function
	.set	nomicromips
	.set	nomips16
	.ent	parms_set
parms_set:                              # @parms_set
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lw	$1, 0($4)
	lui	$2, 65535
	and	$1, $1, $2
	lui	$2, 61
	bne	$1, $2, $BB0_7
	nop
# BB#1:
	b	$BB0_2
	nop
$BB0_2:
	lw	$2, 4($4)
	sltiu	$1, $2, 53
	beqz	$1, $BB0_7
	nop
# BB#3:
	b	$BB0_4
	nop
$BB0_4:
	addiu	$1, $zero, 1
	sllv	$3, $1, $2
	lw	$2, 4($6)
	and	$1, $2, $3
	beqz	$1, $BB0_7
	nop
# BB#5:
	b	$BB0_6
	nop
$BB0_6:
	not	$1, $3
	and	$1, $2, $1
	sw	$1, 4($6)
	lw	$1, 0($6)
	addiu	$1, $1, -1
	sw	$1, 0($6)
$BB0_7:
	jr	$ra
	nop
	.set	at
	.set	macro
	.set	reorder
	.end	parms_set
$func_end0:
	.size	parms_set, ($func_end0)-parms_set


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
