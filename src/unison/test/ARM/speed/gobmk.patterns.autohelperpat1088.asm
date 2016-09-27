	.text
	.syntax unified
	.eabi_attribute	67, "2.09"	@ Tag_conformance
	.cpu	arm1156t2f-s
	.eabi_attribute	6, 8	@ Tag_CPU_arch
	.eabi_attribute	8, 1	@ Tag_ARM_ISA_use
	.eabi_attribute	9, 2	@ Tag_THUMB_ISA_use
	.fpu	vfpv2
	.eabi_attribute	17, 1	@ Tag_ABI_PCS_GOT_use
	.eabi_attribute	20, 1	@ Tag_ABI_FP_denormal
	.eabi_attribute	21, 1	@ Tag_ABI_FP_exceptions
	.eabi_attribute	23, 3	@ Tag_ABI_FP_number_model
	.eabi_attribute	34, 1	@ Tag_CPU_unaligned_access
	.eabi_attribute	24, 1	@ Tag_ABI_align_needed
	.eabi_attribute	25, 1	@ Tag_ABI_align_preserved
	.eabi_attribute	38, 1	@ Tag_ABI_FP_16bit_format
	.eabi_attribute	18, 4	@ Tag_ABI_PCS_wchar_t
	.eabi_attribute	26, 2	@ Tag_ABI_enum_size
	.eabi_attribute	14, 0	@ Tag_ABI_PCS_R9_use
	.file	"gobmk.patterns.autohelperpat1088.ll"
	.hidden	autohelperpat1088
	.globl	autohelperpat1088
	.align	1
	.type	autohelperpat1088,%function
	.code	16                      @ @autohelperpat1088
	.thumb_func
autohelperpat1088:
	.fnstart
@ BB#0:
	.save	{r4, r6, r7, lr}
	push	{r4, r6, r7, lr}
	.setfp	r7, sp, #8
	add	r7, sp, #8
	.pad	#8
	sub	sp, #8
	movw	r3, :lower16:transformation
	movt	r3, :upper16:transformation
	add.w	r0, r3, r0, lsl #2
	movw	r3, #21856
	ldr	r3, [r0, r3]
	mov.w	r4, #23040
	ldr	r0, [r0, r4]
	add	r3, r1
	str	r3, [sp]
	adds	r3, r0, r1
	movs	r4, #0
	mov	r0, r2
	movs	r1, #0
	movs	r2, #1
	bl	play_attack_defend_n
	cmp	r0, #0
	it	eq
	moveq	r4, #1
	mov	r0, r4
	add	sp, #8
	pop	{r4, r6, r7, pc}
.Lfunc_end0:
	.size	autohelperpat1088, .Lfunc_end0-autohelperpat1088
	.cantunwind
	.fnend


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",%progbits
	.eabi_attribute	30, 2	@ Tag_ABI_optimization_goals
