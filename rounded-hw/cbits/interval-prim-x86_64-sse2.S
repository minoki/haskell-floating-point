/* NB: We need some tricks to include "ghcconfig.h" from assembly source files. See Setup.hs for details. */
#include "ghcconfig.h"
#if LEADING_UNDERSCORE
#define SYMBOL2(name) _##name
#define SYMBOL(name) SYMBOL2(name)
#else
#define SYMBOL(name) name
#endif

#define USE_STATIC_MXCSR

    # MXCSR value:
    # 0b0_01_111111_0_000000 = 0x3f80
	#   ^ ^^ ^^^^^^ ^ ^^^^^^
	#   |  |    |   |    +--- Flags: PE,UE,OE,ZE,DE,IE
	#   |  |    |   +-------- DAZ (Denormals are Zeros)
	#   |  |    +------------ Masks: PM,UM,OM,ZM,DM,IM
	#   |  +----------------- Rounding Control
	#   +-------------------- Flush to Zero
	.p2align 3
L_MXCSR_DOWNWARD:
    .long 0x00003f80
L_MXCSR_UPWARD:
    # 0b0_10_111111_0_000000
    .long 0x00005f80

	.globl SYMBOL(rounded_hw_interval_backend_name)
SYMBOL(rounded_hw_interval_backend_name):
	.string "SSE2"

#define PREV_MXCSR 8(%rsp)
#define TMP_MXCSR 12(%rsp)

	#
	# rounded_hw_interval_add
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> Double# -- lower 2 (%xmm3)
	#   -> Double# -- upper 2 (%xmm4)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.p2align 4
	.globl SYMBOL(rounded_hw_interval_add)
SYMBOL(rounded_hw_interval_add):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_DOWNWARD(%rip)
#else
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
#endif
	addsd %xmm3, %xmm1     # xmm1 = xmm1[0] + xmm3[0], xmm1[1]
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_UPWARD(%rip)
#else
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
#endif
	addsd %xmm4, %xmm2     # xmm2 = xmm2[0] + xmm4[0], xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR
#if defined(TABLES_NEXT_TO_CODE)
	jmp *(%rbp)
#else
	movq (%rbp), %rax
	jmp *(%rax)
#endif

	#
	# rounded_hw_interval_sub
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> Double# -- lower 2 (%xmm3)
	#   -> Double# -- upper 2 (%xmm4)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.p2align 4
	.globl SYMBOL(rounded_hw_interval_sub)
SYMBOL(rounded_hw_interval_sub):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_DOWNWARD(%rip)
#else
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
#endif
	subsd %xmm4, %xmm1     # xmm1 = xmm1[0] - xmm4[0], xmm1[1]
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_UPWARD(%rip)
#else
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
#endif
	subsd %xmm3, %xmm2     # xmm2 = xmm2[0] - xmm3[0], xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR
#if defined(TABLES_NEXT_TO_CODE)
	jmp *(%rbp)
#else
	movq (%rbp), %rax
	jmp *(%rax)
#endif

	#
	# rounded_hw_interval_recip
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.p2align 4
	.globl SYMBOL(rounded_hw_interval_recip)
SYMBOL(rounded_hw_interval_recip):
	stmxcsr PREV_MXCSR      # PREV_MXCSR = MXCSR
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_DOWNWARD(%rip)
#else
	movl PREV_MXCSR, %ecx   # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx      # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx       # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR    # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR       # MXCSR = TMP_MXCSR; set downward
#endif
	movsd LC0(%rip), %xmm3  # xmm3 = (double)1.0,zero
	movapd %xmm3, %xmm4     # xmm4 = xmm3
	divsd %xmm2, %xmm3      # xmm3 = xmm3[0] / xmm2[0], xmm3[1]
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_UPWARD(%rip)
#else
	xorl $0x6000, %ecx      # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR    # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR       # MXCSR = TMP_MXCSR; set upward
#endif
	divsd %xmm1, %xmm4      # xmm4 = xmm4[0] / xmm1[0], xmm4[1]
	ldmxcsr PREV_MXCSR      # MXCSR = PREV_MXCSR; restore
	movapd %xmm3, %xmm1     # xmm1 = xmm3
	movapd %xmm4, %xmm2     # xmm2 = xmm4
#if defined(TABLES_NEXT_TO_CODE)
	jmp *(%rbp)
#else
	movq (%rbp), %rax
	jmp *(%rax)
#endif
LC0:
	.quad 0x3FF0000000000000   # 1.0 in binary64
	# 0b0011_1111_1111_0000_..._0000
	#   ^^----+------^ ^-----+-----^
	#   |     |              +-- trailing significand field
	#   |     +-- biased exponent
	#   +-- sign

	#
	# rounded_hw_interval_sqrt
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.p2align 4
	.globl SYMBOL(rounded_hw_interval_sqrt)
SYMBOL(rounded_hw_interval_sqrt):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_DOWNWARD(%rip)
#else
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set downward
#endif
	sqrtsd %xmm1, %xmm1    # xmm1 = sqrt(xmm1[0]), xmm1[1]
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_UPWARD(%rip)
#else
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set upward
#endif
	sqrtsd %xmm2, %xmm2    # xmm2 = sqrt(xmm2[0]), xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR; restore
#if defined(TABLES_NEXT_TO_CODE)
	jmp *(%rbp)
#else
	movq (%rbp), %rax
	jmp *(%rax)
#endif

	#
	# rounded_hw_interval_from_int64
	#   :: Int(64)# -- input (%rbx)
	#   -> (# Double# -- lower (%xmm1)
	#       , Double# -- upper (%xmm2)
	#       #)
	#
	.p2align 4
	.globl SYMBOL(rounded_hw_interval_from_int64)
SYMBOL(rounded_hw_interval_from_int64):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_DOWNWARD(%rip)
#else
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set downward
#endif
	pxor %xmm1, %xmm1      # xmm1 = zero
	cvtsi2sdq %rbx, %xmm1  # xmm1 = (double)(int64)rbx, xmm1[1]
#if defined(USE_STATIC_MXCSR)
	ldmxcsr L_MXCSR_UPWARD(%rip)
#else
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set upward
#endif
	pxor %xmm2, %xmm2      # xmm2 = zero
	cvtsi2sdq %rbx, %xmm2  # xmm2 = (double)(int64)rbx, xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR; restore
#if defined(TABLES_NEXT_TO_CODE)
	jmp *(%rbp)
#else
	movq (%rbp), %rax
	jmp *(%rax)
#endif
