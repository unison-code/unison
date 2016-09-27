	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"h264ref.vlc.symbol2uvlc.ll"
	.text
	.globl	symbol2uvlc
	.align	2
	.type	symbol2uvlc,@function
	.set	nomicromips
	.set	nomips16
	.ent	symbol2uvlc
symbol2uvlc:                            # @symbol2uvlc
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lw	$1, 12($4)
	srl	$2, $1, 31
	addu	$1, $1, $2
	sra	$1, $1, 1
	addiu	$2, $zero, 1
	sllv	$1, $2, $1
	lw	$2, 16($4)
	addiu	$3, $1, -1
	and	$2, $3, $2
	or	$1, $2, $1
	sw	$1, 20($4)
	jr	$ra
	addiu	$2, $zero, 0
	.set	at
	.set	macro
	.set	reorder
	.end	symbol2uvlc
$func_end0:
	.size	symbol2uvlc, ($func_end0)-symbol2uvlc


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
