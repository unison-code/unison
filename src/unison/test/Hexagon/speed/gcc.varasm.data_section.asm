	.text
	.file	"gcc.varasm.data_section.ll"
	.globl	data_section
	.align	16
	.type	data_section,@function
data_section:                           // @data_section
// BB#0:
	{
			r0 = memb(##in_section)
			if (cmp.eq(r0.new, #2)) jump:t .LBB0_2
	}
	{
			jump .LBB0_1
	}
.LBB0_1:
	{
			allocframe(#8)
	}
	{
			r2 = ##.str.3
			r0 = memw(##asm_out_file)
	}
	{
			call fprintf
			r1 = ##.str
			memw(r29 + #0) = r2
	}
	{
			r0 = #2
			memb(###in_section) = r0.new
	}
	{
			deallocframe
	}
.LBB0_2:
	{
			jumpr r31
	}
.Lfunc_end0:
	.size	data_section, .Lfunc_end0-data_section

	.hidden	in_section
	.hidden	.str
	.hidden	.str.3

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
