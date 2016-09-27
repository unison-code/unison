	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"sphinx3.profile.ptmr_init.ll"
	.text
	.globl	ptmr_init
	.align	2
	.type	ptmr_init,@function
	.set	nomicromips
	.set	nomips16
	.ent	ptmr_init
ptmr_init:                              # @ptmr_init
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	sw	$zero, 36($4)
	sw	$zero, 32($4)
	sw	$zero, 28($4)
	sw	$zero, 24($4)
	sw	$zero, 20($4)
	sw	$zero, 16($4)
	sw	$zero, 12($4)
	jr	$ra
	sw	$zero, 8($4)
	.set	at
	.set	macro
	.set	reorder
	.end	ptmr_init
$func_end0:
	.size	ptmr_init, ($func_end0)-ptmr_init


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
