	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gobmk.barriers.autohelperbarrierspat145.ll"
	.text
	.hidden	autohelperbarrierspat145
	.globl	autohelperbarrierspat145
	.align	2
	.type	autohelperbarrierspat145,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperbarrierspat145
autohelperbarrierspat145:               # @autohelperbarrierspat145
	.frame	$sp,40,$ra
	.mask 	0x800f0000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, %hi(_gp_disp)
	addiu	$2, $2, %lo(_gp_disp)
	addiu	$sp, $sp, -40
	sw	$ra, 36($sp)            # 4-byte Folded Spill
	sw	$19, 32($sp)            # 4-byte Folded Spill
	sw	$18, 28($sp)            # 4-byte Folded Spill
	sw	$17, 24($sp)            # 4-byte Folded Spill
	sw	$16, 20($sp)            # 4-byte Folded Spill
	addu	$16, $2, $25
	move	 $17, $6
	move	 $18, $5
	sll	$1, $4, 2
	lw	$2, %got(transformation)($16)
	addu	$1, $2, $1
	lw	$19, 20736($1)
	lw	$1, 19552($1)
	lw	$25, %call16(influence_mark_non_territory)($16)
	addu	$4, $1, $18
	move	 $5, $17
	jalr	$25
	move	 $gp, $16
	addu	$4, $19, $18
	lw	$25, %call16(influence_mark_non_territory)($16)
	jalr	$25
	move	 $5, $17
	addiu	$2, $zero, 0
	lw	$16, 20($sp)            # 4-byte Folded Reload
	lw	$17, 24($sp)            # 4-byte Folded Reload
	lw	$18, 28($sp)            # 4-byte Folded Reload
	lw	$19, 32($sp)            # 4-byte Folded Reload
	lw	$ra, 36($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 40
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperbarrierspat145
$func_end0:
	.size	autohelperbarrierspat145, ($func_end0)-autohelperbarrierspat145


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
