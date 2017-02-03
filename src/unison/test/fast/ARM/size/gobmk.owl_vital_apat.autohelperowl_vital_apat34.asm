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
	.file	"gobmk.owl_vital_apat.autohelperowl_vital_apat34.ll"
	.hidden	autohelperowl_vital_apat34
	.globl	autohelperowl_vital_apat34
	.align	1
	.type	autohelperowl_vital_apat34,%function
	.code	16                      @ @autohelperowl_vital_apat34
	.thumb_func
autohelperowl_vital_apat34:
	.fnstart
@ BB#0:
	.save	{r7, lr}
	push	{r7, lr}
	.setfp	r7, sp
	mov	r7, sp
	.pad	#8
	sub	sp, #8
	mov	r3, r1
	movw	r1, :lower16:transformation
	movt	r1, :upper16:transformation
	add.w	r0, r1, r0, lsl #2
	movw	r1, #24256
	ldr	r0, [r0, r1]
	add	r0, r3
	strd	r0, r3, [sp]
	mov	r0, r2
	movs	r1, #0
	movs	r2, #1
	bl	play_attack_defend2_n
	add	sp, #8
	pop	{r7, pc}
.Lfunc_end0:
	.size	autohelperowl_vital_apat34, .Lfunc_end0-autohelperowl_vital_apat34
	.cantunwind
	.fnend


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",%progbits
	.eabi_attribute	30, 1	@ Tag_ABI_optimization_goals
