	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"sphinx3.glist.glist_tail.ll"
	.text
	.globl	glist_tail
	.align	2
	.type	glist_tail,@function
	.set	nomicromips
	.set	nomips16
	.ent	glist_tail
glist_tail:                             # @glist_tail
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	beqz	$4, $BB0_4
	addiu	$2, $zero, 0
# BB#1:
	.insn
$BB0_2:                                 # %.preheader
                                        # =>This Inner Loop Header: Depth=1
	move	 $2, $4
	lw	$4, 8($2)
	bnez	$4, $BB0_2
	nop
# BB#3:                                 # %.preheader
	b	$BB0_4
	nop
$BB0_4:                                 # %.loopexit
	jr	$ra
	nop
	.set	at
	.set	macro
	.set	reorder
	.end	glist_tail
$func_end0:
	.size	glist_tail, ($func_end0)-glist_tail


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
