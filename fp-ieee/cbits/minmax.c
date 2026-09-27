
// In case of GCC, -fsignaling-nans must be set to use '*= 1.0' as canonicalization
// #if defined(__GNUC__) && !defined(__SUPPORT_SNAN__)
// #error "-fsignaling-nans must be set"
// #endif

#if defined(__aarch64__)

// Properties of minimum and maximum:
// * -0 < +0
// * If either of inputs is NaN, returns a quiet NaN.

float hs_minimumFloat(float x, float y)
{
    float result;
    __asm__("fmin %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

float hs_maximumFloat(float x, float y)
{
    float result;
    __asm__("fmax %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

double hs_minimumDouble(double x, double y)
{
    double result;
    __asm__("fmin %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

double hs_maximumDouble(double x, double y)
{
    double result;
    __asm__("fmax %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

// Properties of minimumNumber and maximumNumber:
// * -0 < +0
// * Treat a NaN as "lack of input".
//   If both of inputs are NaNs, returns a quiet NaN.

float hs_minimumNumberFloat(float x, float y)
{
    float result;
    // FMINNM always returns a NaN if either of inputs is signaling NaN.
    // Therefore, we convert signaling NaNs to quiet ones before applying FMINNM.
    // x *= 1.0f;
    // y *= 1.0f;
    __asm__("fmul %s0, %s0, %s1" : "+w"(x) : "w"(1.0f));
    __asm__("fmul %s0, %s0, %s1" : "+w"(y) : "w"(1.0f));
    __asm__("fminnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

float hs_maximumNumberFloat(float x, float y)
{
    float result;
    // FMAXNM always returns a NaN if either of inputs is signaling NaN.
    // Therefore, we convert signaling NaNs to quiet ones before applying FMAXNM.
    // x *= 1.0f;
    // y *= 1.0f;
    __asm__("fmul %s0, %s0, %s1" : "+w"(x) : "w"(1.0f));
    __asm__("fmul %s0, %s0, %s1" : "+w"(y) : "w"(1.0f));
    __asm__("fmaxnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

double hs_minimumNumberDouble(double x, double y)
{
    double result;
    // FMINNM always returns a NaN if either of inputs is signaling NaN.
    // Therefore, we convert signaling NaNs to quiet ones before applying FMINNM.
    // x *= 1.0;
    // y *= 1.0;
    __asm__("fmul %d0, %d0, %d1" : "+w"(x) : "w"(1.0));
    __asm__("fmul %d0, %d0, %d1" : "+w"(y) : "w"(1.0));
    __asm__("fminnm %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

double hs_maximumNumberDouble(double x, double y)
{
    double result;
    // FMAXNM always returns a NaN if either of inputs is signaling NaN.
    // Therefore, we convert signaling NaNs to quiet ones before applying FMAXNM.
    // x *= 1.0;
    // y *= 1.0;
    __asm__("fmul %d0, %d0, %d1" : "+w"(x) : "w"(1.0));
    __asm__("fmul %d0, %d0, %d1" : "+w"(y) : "w"(1.0));
    __asm__("fmaxnm %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}

#elif defined(__riscv)

#if defined(__riscv_zfa)
float hs_minimumFloat(float x, float y)
{
    float result;
    __asm__("fminm.s %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

float hs_maximumFloat(float x, float y)
{
    float result;
    __asm__("fmaxm.s %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

double hs_minimumDouble(double x, double y)
{
    double result;
    __asm__("fminm.d %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

double hs_maximumDouble(double x, double y)
{
    double result;
    __asm__("fmaxm.d %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}
#endif

float hs_minimumNumberFloat(float x, float y)
{
    float result;
    __asm__("fmin.s %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

float hs_maximumNumberFloat(float x, float y)
{
    float result;
    __asm__("fmax.s %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

double hs_minimumNumberDouble(double x, double y)
{
    double result;
    __asm__("fmin.d %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

double hs_maximumNumberDouble(double x, double y)
{
    double result;
    __asm__("fmax.d %0, %1, %2" : "=f"(result) : "f"(x), "f"(y));
    return result;
}

#else

#error "Unsupported platform"

#endif
