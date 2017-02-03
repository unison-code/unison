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
	.file	"gcc.alias.get_frame_alias_set.ll"
	.globl	get_frame_alias_set
	.align	1
	.type	get_frame_alias_set,%function
	.code	16                      @ @get_frame_alias_set
	.thumb_func
get_frame_alias_set:
	.fnstart
@ BB#0:
	movw	r1, :lower16:get_frame_alias_set.set
	movt	r1, :upper16:get_frame_alias_set.set
	ldr	r0, [r1]
	cmp.w	r0, #-1
	bne.w	.LBB0_5
	b.w	.LBB0_1
.LBB0_1:
	movw	r0, :lower16:flag_strict_aliasing
	movt	r0, :upper16:flag_strict_aliasing
	ldr	r0, [r0]
	cmp	r0, #0
	bne.w	.LBB0_3
@ BB#2:
	movs	r0, #0
	b.w	.LBB0_4
.LBB0_3:
	movw	r2, :lower16:new_alias_set.last_alias_set
	movt	r2, :upper16:new_alias_set.last_alias_set
	ldr	r0, [r2]
	adds	r0, #1
	str	r0, [r2]
.LBB0_4:                                @ %new_alias_set.exit
	str	r0, [r1]
.LBB0_5:
	bx	lr
.Lfunc_end0:
	.size	get_frame_alias_set, .Lfunc_end0-get_frame_alias_set
	.cantunwind
	.fnend

	.hidden	new_alias_set.last_alias_set
	.hidden	get_frame_alias_set.set

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",%progbits
	.eabi_attribute	30, 1	@ Tag_ABI_optimization_goals
