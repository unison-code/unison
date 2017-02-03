	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.owl_defendpat.autohelperowl_defendpat421.ll"
	.text
	.hidden	autohelperowl_defendpat421
	.globl	autohelperowl_defendpat421
	.align	2
	.type	autohelperowl_defendpat421,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperowl_defendpat421
autohelperowl_defendpat421:             # @autohelperowl_defendpat421
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
	sll	$1, $4, 2
	lw	$2, %got(transformation)($gp)
	addu	$1, $2, $1
	lw	$2, 18336($1)
	addiu	$1, $zero, 3
	subu	$1, $1, $6
	addu	$4, $2, $5
	lw	$25, %call16(safe_move)($gp)
	jalr	$25
	move	 $5, $1
	sltiu	$2, $2, 1
	lw	$ra, 20($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 24
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperowl_defendpat421
$func_end0:
	.size	autohelperowl_defendpat421, ($func_end0)-autohelperowl_defendpat421


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
