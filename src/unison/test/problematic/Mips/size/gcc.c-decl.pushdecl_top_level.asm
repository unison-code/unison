	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"gcc.c-decl.pushdecl_top_level.ll"
	.text
	.globl	pushdecl_top_level
	.align	2
	.type	pushdecl_top_level,@function
	.set	nomicromips
	.set	nomips16
	.ent	pushdecl_top_level
pushdecl_top_level:                     # @pushdecl_top_level
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
	lw	$16, %got(current_binding_level)($gp)
	lw	$17, 0($16)
	lw	$1, %got(global_binding_level)($gp)
	lw	$1, 0($1)
	sw	$1, 0($16)
	lw	$25, %call16(pushdecl)($gp)
	jalr	$25
	nop
	sw	$17, 0($16)
	lw	$16, 20($sp)            # 4-byte Folded Reload
	lw	$17, 24($sp)            # 4-byte Folded Reload
	lw	$ra, 28($sp)            # 4-byte Folded Reload
	jr	$ra
	addiu	$sp, $sp, 32
	.set	at
	.set	macro
	.set	reorder
	.end	pushdecl_top_level
$func_end0:
	.size	pushdecl_top_level, ($func_end0)-pushdecl_top_level

	.hidden	current_binding_level
	.hidden	global_binding_level

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
