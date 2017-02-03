	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"h264ref.memalloc.no_mem_exit.ll"
	.text
	.globl	no_mem_exit
	.align	2
	.type	no_mem_exit,@function
	.set	nomicromips
	.set	nomips16
	.ent	no_mem_exit
no_mem_exit:                            # @no_mem_exit
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
	move	 $1, $4
	lw	$17, %got(errortext)($16)
	lw	$6, %got(.str.20)($16)
	lw	$25, %call16(snprintf)($16)
	move	 $4, $17
	addiu	$5, $zero, 300
	move	 $7, $1
	jalr	$25
	move	 $gp, $16
	lw	$25, %call16(error)($16)
	move	 $4, $17
	addiu	$5, $zero, 100
	jalr	$25
	move	 $gp, $16
	lw	$16, 20($sp)            # 4-byte Folded Reload
	lw	$17, 24($sp)            # 4-byte Folded Reload
	lw	$ra, 28($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 32
	.set	at
	.set	macro
	.set	reorder
	.end	no_mem_exit
$func_end0:
	.size	no_mem_exit, ($func_end0)-no_mem_exit

	.hidden	.str.20

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
