	.text
	.file	"h264ref.vlc.symbol2uvlc.ll"
	.globl	symbol2uvlc
	.align	16
	.type	symbol2uvlc,@function
symbol2uvlc:                            // @symbol2uvlc
// BB#0:
	{
			r1:0 = combine(r0, #-1)
			r2 = #1
	}
	{
			r3 = memw(r1 + #12)
	}
	{
			r3 += lsr(r3, #31)
	}
	{
			r3 = asr(r3, #1)
			r4 = memw(r1 + #16)
	}
	{
			r0 += asl(r2, r3)
	}
	{
			r2 = and(r0, r4)
			r0 = #0
	}
	{
			r2 = setbit(r2, r3)
			jumpr r31
			memw(r1+#20) = r2.new
	}
.Lfunc_end0:
	.size	symbol2uvlc, .Lfunc_end0-symbol2uvlc


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
