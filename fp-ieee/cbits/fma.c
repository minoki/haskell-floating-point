#include <math.h>

#if defined(__GNUC__) && defined(__FMA__)

// Make sure FMA instruction is used even if optimizations are disabled.

double hs_fusedMultiplyAddDouble(double a, double b, double c)
{
    // vfmadd132sd %xmm1, %xmm2, %xmm0
    // %xmm0 <- %xmm0 * %xmm1 + %xmm2
    __asm__("vfmadd132sd %1, %2, %0" : "+x"(a) : "x"(b), "x"(c));
    return a;
}
float hs_fusedMultiplyAddFloat(float a, float b, float c)
{
    // vfmadd132ss %xmm1, %xmm2, %xmm0
    // %xmm0 <- %xmm0 * %xmm1 + %xmm2
    __asm__("vfmadd132ss %1, %2, %0" : "+x"(a) : "x"(b), "x"(c));
    return a;
}

#else

#if !defined(FP_FAST_FMA)
#error "The compiler should define FP_FAST_FMA"
#endif
#if !defined(FP_FAST_FMAF)
#error "The compiler should define FP_FAST_FMAF"
#endif

double hs_fusedMultiplyAddDouble(double a, double b, double c)
{
    return fma(a, b, c);
}
float hs_fusedMultiplyAddFloat(float a, float b, float c)
{
    return fmaf(a, b, c);
}

#endif
