	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.owl_attackpat.autohelperowl_attackpat68.ll"
	.text
	.hidden	autohelperowl_attackpat68
	.globl	autohelperowl_attackpat68
	.align	2
	.type	autohelperowl_attackpat68,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperowl_attackpat68
autohelperowl_attackpat68:              # @autohelperowl_attackpat68
	.frame	$sp,40,$ra
	.mask 	0x80000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addiu	$sp, $sp, -40
	sw	$ra, 36($sp)            # 4-byte Folded Spill
	addu	$gp, $2, $25
	move	 $1, $5
	sll	$2, $4, 2
	lw	$3, %got(transformation)($gp)
	addu	$2, $3, $2
	lw	$3, 21856($2)
	lw	$2, 20672($2)
	sw	$1, 20($sp)
	addu	$2, $2, $1
	sw	$2, 24($sp)
	addu	$2, $3, $1
	sw	$2, 16($sp)
	lw	$25, %call16(play_attack_defend2_n)($gp)
	move	 $4, $6
	addiu	$5, $zero, 0
	addiu	$6, $zero, 2
	jalr	$25
	move	 $7, $1
	lw	$ra, 36($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 40
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperowl_attackpat68
$func_end0:
	.size	autohelperowl_attackpat68, ($func_end0)-autohelperowl_attackpat68


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
