#include <math.h>
#include <fenv.h>

#if defined(__SSE4_1__) // SSE 4.1

#include <x86intrin.h>

float hs_fp_ieee_roundevenFloat(float x)
{
    __m128 xv = _mm_set_ss(x);
    xv = _mm_round_ss(xv, xv, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    float result;
    _mm_store_ss(&result, xv);
    return result;
}

double hs_fp_ieee_roundevenDouble(double x)
{
    __m128d xv = _mm_set_sd(x);
    xv = _mm_round_sd(xv, xv, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    double result;
    _mm_store_sd(&result, xv);
    return result;
}

#elif defined(__aarch64__) // ARMv8-A

float hs_fp_ieee_roundevenFloat(float x)
{
    float result;
    // a floating-exception can be generated
    __asm__("frintn %s0, %s1" : "=w"(result) : "w"(x));
    return result;
}

double hs_fp_ieee_roundevenDouble(double x)
{
    double result;
    // a floating-exception can be generated
    __asm__("frintn %d0, %d1" : "=w"(result) : "w"(x));
    return result;
}

#elif defined(__riscv_zfa)

float hs_fp_ieee_roundevenFloat(float x)
{
    float result;
    // a floating-exception can be generated for sNaN
    __asm__("fround.s %0, %1, rne" : "=f"(result) : "f"(x));
    return result;
}

double hs_fp_ieee_roundevenDouble(double x)
{
    double result;
    // a floating-exception can be generated for sNaN
    __asm__("fround.d %0, %1, rne" : "=f"(result) : "f"(x));
    return result;
}

#else

#error "Unsupported architecture"

#endif
