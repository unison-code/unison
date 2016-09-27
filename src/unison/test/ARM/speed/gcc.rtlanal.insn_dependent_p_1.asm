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
	.file	"gcc.rtlanal.insn_dependent_p_1.ll"
	.hidden	insn_dependent_p_1
	.globl	insn_dependent_p_1
	.align	1
	.type	insn_dependent_p_1,%function
	.code	16                      @ @insn_dependent_p_1
	.thumb_func
insn_dependent_p_1:
	.fnstart
@ BB#0:
	.save	{r4, r6, r7, lr}
	push	{r4, r6, r7, lr}
	.setfp	r7, sp, #8
	add	r7, sp, #8
	mov	r4, r2
	ldr	r1, [r4]
	cmp	r1, #0
	beq.w	.LBB0_3
	b.w	.LBB0_1
.LBB0_1:
	bl	reg_mentioned_p
	cmp	r0, #0
	beq.w	.LBB0_3
	b.w	.LBB0_2
.LBB0_2:
	movs	r0, #0
	str	r0, [r4]
.LBB0_3:
	pop	{r4, r6, r7, pc}
.Lfunc_end0:
	.size	insn_dependent_p_1, .Lfunc_end0-insn_dependent_p_1
	.cantunwind
	.fnend


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",%progbits
	.eabi_attribute	30, 2	@ Tag_ABI_optimization_goals
