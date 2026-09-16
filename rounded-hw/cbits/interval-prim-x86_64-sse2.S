/* NB: We need some tricks to include "ghcconfig.h" from assembly source files. See Setup.hs for details. */
#include "ghcconfig.h"
#if LEADING_UNDERSCORE
#define SYMBOL2(name) _##name
#define SYMBOL(name) SYMBOL2(name)
#else
#define SYMBOL(name) name
#endif

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
	.globl SYMBOL(rounded_hw_interval_add)
SYMBOL(rounded_hw_interval_add):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
	addsd %xmm3, %xmm1     # xmm1 = xmm1[0] + xmm3[0], xmm1[1]
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
	addsd %xmm4, %xmm2     # xmm2 = xmm2[0] + xmm4[0], xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR
	jmp *(%rbp)

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
	.globl SYMBOL(rounded_hw_interval_sub)
SYMBOL(rounded_hw_interval_sub):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
	subsd %xmm4, %xmm1     # xmm1 = xmm1[0] - xmm4[0], xmm1[1]
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR
	subsd %xmm3, %xmm2     # xmm2 = xmm2[0] - xmm3[0], xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR
	jmp *(%rbp)

	#
	# rounded_hw_interval_recip
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_recip)
SYMBOL(rounded_hw_interval_recip):
	stmxcsr PREV_MXCSR      # PREV_MXCSR = MXCSR
	movl PREV_MXCSR, %ecx   # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx      # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx       # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR    # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR       # MXCSR = TMP_MXCSR; set downward
	movsd LC0(%rip), %xmm3  # xmm3 = (double)1.0,zero
	movapd %xmm3, %xmm4     # xmm4 = xmm3
	divsd %xmm2, %xmm3      # xmm3 = xmm3[0] / xmm2[0], xmm3[1]
	xorl $0x6000, %ecx      # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR    # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR       # MXCSR = TMP_MXCSR; set upward
	divsd %xmm1, %xmm4      # xmm4 = xmm4[0] / xmm1[0], xmm4[1]
	ldmxcsr PREV_MXCSR      # MXCSR = PREV_MXCSR; restore
	movapd %xmm3, %xmm1     # xmm1 = xmm3
	movapd %xmm4, %xmm2     # xmm2 = xmm4
	jmp *(%rbp)
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
	.globl SYMBOL(rounded_hw_interval_sqrt)
SYMBOL(rounded_hw_interval_sqrt):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set downward
	sqrtsd %xmm1, %xmm1    # xmm1 = sqrt(xmm1[0]), xmm1[1]
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set upward
	sqrtsd %xmm2, %xmm2    # xmm2 = sqrt(xmm2[0]), xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR; restore
	jmp *(%rbp)

	#
	# rounded_hw_interval_from_int64
	#   :: Int(64)# -- input (%rbx)
	#   -> (# Double# -- lower (%xmm1)
	#       , Double# -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_from_int64)
SYMBOL(rounded_hw_interval_from_int64):
	stmxcsr PREV_MXCSR     # PREV_MXCSR = MXCSR
	movl PREV_MXCSR, %ecx  # ecx = PREV_MXCSR
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set downward
	pxor %xmm1, %xmm1      # xmm1 = zero
	cvtsi2sdq %rbx, %xmm1  # xmm1 = (double)(int64)rbx, xmm1[1]
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, TMP_MXCSR   # TMP_MXCSR = ecx
	ldmxcsr TMP_MXCSR      # MXCSR = TMP_MXCSR; set upward
	pxor %xmm2, %xmm2      # xmm2 = zero
	cvtsi2sdq %rbx, %xmm2  # xmm2 = (double)(int64)rbx, xmm2[1]
	ldmxcsr PREV_MXCSR     # MXCSR = PREV_MXCSR; restore
	jmp *(%rbp)
