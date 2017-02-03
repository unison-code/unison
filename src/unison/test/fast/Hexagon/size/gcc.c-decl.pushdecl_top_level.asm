	.text
	.file	"gcc.c-decl.pushdecl_top_level.ll"
	.globl	pushdecl_top_level
	.align	16
	.type	pushdecl_top_level,@function
pushdecl_top_level:                     // @pushdecl_top_level
// BB#0:
	{
			memd(r29 + #-16) = r17:16
			allocframe(#8)
	}
	{
			r1 = memw(##global_binding_level)
			r16 = memw(##current_binding_level)
	}
	{
			call pushdecl
			memw(##current_binding_level) = r1
	}
	{
			r17:16 = memd(r29 + #0)
			memw(##current_binding_level) = r16
	}
	{
			dealloc_return
	}
.Lfunc_end0:
	.size	pushdecl_top_level, .Lfunc_end0-pushdecl_top_level

	.hidden	current_binding_level
	.hidden	global_binding_level

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
