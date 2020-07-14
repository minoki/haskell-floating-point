#include <math.h>
#include <fenv.h>

// __SSE4_1__
#if defined(__SSE4_1__)

#include <x86intrin.h>

// const char hs_roundeven_impl[] = "SSE4.1";

float hs_roundevenFloat(float x)
{
    __m128 xv = _mm_set_ss(x);
    xv = _mm_round_ss(xv, xv, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    float result;
    _mm_store_ss(&result, xv);
    return result;
}

double hs_roundevenDouble(double x)
{
    __m128d xv = _mm_set_sd(x);
    xv = _mm_round_sd(xv, xv, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    double result;
    _mm_store_sd(&result, xv);
    return result;
}

#elif defined(__aarch64__) // ARMv8-A

// const char hs_roundeven_impl[] = "AArch64 FRINTN";

float hs_roundevenFloat(float x)
{
    float result;
    // a floating-exception can be generated
    asm("frintn %1, %0" : "r"(x) : "=r"(result));
    return result;
}

double hs_roundevenDouble(double x)
{
    double result;
    // a floating-exception can be generated
    asm("frintn %1, %0" : "r"(x) : "=r"(result));
    return result;
}

#else

#error "Unsupported architecture"

#endif
