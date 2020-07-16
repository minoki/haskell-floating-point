#include <math.h>

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
float hs_fusedMultiplyAddDouble(float a, float b, float c)
{
    return fmaf(a, b, c);
}
