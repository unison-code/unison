	.text
	.file	"h264ref.sei.UpdateRandomAccess.ll"
	.globl	UpdateRandomAccess
	.align	16
	.type	UpdateRandomAccess,@function
UpdateRandomAccess:                     // @UpdateRandomAccess
// BB#0:
	{
			r1 = #0
			r0 = memw(##img)
	}
	{
			r0 = memw(r0 + #24)
	}
	{
			p0 = cmp.eq(r0, #2)
			if (p0.new) r0 = #1
			r2 = #256
			if (!p0.new) r0 = r1
	}
	{
			if (p0) memh(##seiRandomAccess) = r2
			if (p0) memb(##seiRandomAccess+2) = r1
	}
	{
			jumpr r31
			memb(##seiHasRandomAccess_info) = r0
	}
.Lfunc_end0:
	.size	UpdateRandomAccess, .Lfunc_end0-UpdateRandomAccess


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
