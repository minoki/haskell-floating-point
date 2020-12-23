#include <math.h>

#pragma STDC FENV_ACCESS ON

#if defined(__SSE2__)

#include <x86intrin.h>

float hs_canonicalizeFloat(float x)
{
    asm volatile("mulss %1, %0" : "+x"(x) : "x"(1.0f));
    return x;
    /*
    Clang optimizes away this:
    __m128 xv = _mm_set_ss(x);
    __m128 onev = _mm_set_ss(1.0f);
    __m128 resultv = _mm_mul_ss(xv, onev);
    float result;
    _mm_store_ss(&result, resultv);
    return result;
    */
}
double hs_canonicalizeDouble(double x)
{
    asm volatile("mulsd %1, %0" : "+x"(x) : "x"(1.0));
    return x;
    /*
    Clang optimizes away this:
    __m128d xv = _mm_set_sd(x);
    __m128d onev = _mm_set_sd(1.0);
    __m128d resultv = _mm_mul_sd(xv, onev);
    double result;
    _mm_store_sd(&result, resultv);
    return result;
    */
}

#elif defined(__aarch64__)

float hs_canonicalizeFloat(float x)
{
    asm volatile("fmul %s0, %s0, %s1" : "+w"(x) : "w"(1.0f));
    return x;
}
double hs_canonicalizeDouble(double x)
{
    asm volatile("fmul %d0, %d0, %d1" : "+w"(x) : "w"(1.0));
    return x;
}

#else

float hs_canonicalizeFloat(float x)
{
    volatile float one = 1.0f;
    return x * one;
}
double hs_canonicalizeDouble(double x)
{
    volatile double one = 1.0;
    return x * one;
}

#endif
