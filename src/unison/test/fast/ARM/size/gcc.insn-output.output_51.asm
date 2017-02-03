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
	.file	"gcc.insn-output.output_51.ll"
	.hidden	output_51
	.globl	output_51
	.align	1
	.type	output_51,%function
	.code	16                      @ @output_51
	.thumb_func
output_51:
	.fnstart
@ BB#0:
	.save	{r4, r6, r7, lr}
	push	{r4, r6, r7, lr}
	.setfp	r7, sp, #8
	add	r7, sp, #8
	mov	r4, r1
	mov	r0, r4
	bl	get_attr_type
	cmp	r0, #8
	bne.w	.LBB0_2
@ BB#1:
	movw	r0, :lower16:.str.2014
	movt	r0, :upper16:.str.2014
	b.w	.LBB0_3
.LBB0_2:
	mov	r0, r4
	bl	get_attr_mode
	mov	r1, r0
	movw	r2, :lower16:.str.2015
	movt	r2, :upper16:.str.2015
	movw	r0, :lower16:.str.75
	movt	r0, :upper16:.str.75
	cmp	r1, #4
	it	eq
	moveq	r0, r2
.LBB0_3:
	pop	{r4, r6, r7, pc}
.Lfunc_end0:
	.size	output_51, .Lfunc_end0-output_51
	.cantunwind
	.fnend

	.hidden	.str.75
	.hidden	.str.2014
	.hidden	.str.2015

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",%progbits
	.eabi_attribute	30, 1	@ Tag_ABI_optimization_goals
