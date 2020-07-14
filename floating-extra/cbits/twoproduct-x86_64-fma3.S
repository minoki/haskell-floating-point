/* NB: We need some tricks to include "ghcconfig.h" from assembly source files. See Setup.hs for details. */
#include "ghcconfig.h"
#if LEADING_UNDERSCORE
#define L2(name) _##name
#define L(name) L2(name)
#else
#define L(name) name
#endif

	.globl L(hs_twoProductDouble)
L(hs_twoProductDouble):
	# input: xmm1, xmm2
	vmulsd %xmm1, %xmm2, %xmm3       # xmm3 = xmm1 * xmm2
	vfmsub213sd %xmm3, %xmm1, %xmm2  # xmm2 = (xmm1 * xmm2) - xmm3
	vmovapd %xmm3, %xmm1             # xmm1 = xmm3
	# output: xmm1, xmm2
	jmp *(%rbp)

	.globl L(hs_twoProductFloat)
L(hs_twoProductFloat):
	# input: xmm1, xmm2
	vmulss %xmm1, %xmm2, %xmm3       # xmm3 = xmm1 * xmm2
	vfmsub213ss %xmm3, %xmm1, %xmm2  # xmm2 = (xmm1 * xmm2) - xmm3
	vmovaps %xmm3, %xmm1             # xmm1 = xmm3
	# output: xmm1, xmm2
	jmp *(%rbp)
