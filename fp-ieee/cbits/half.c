#include <stdint.h> // uint16_t
#include <math.h>

#if defined(__F16C__) // x86 F16C

#include <x86intrin.h>

uint16_t hs_fastFloatToHalf(float f)
{
    __m128 x = _mm_set_ss(f);
    union {
        __m128i v;
        uint16_t c;
    } u;
    // A floating-point exception can be raised
    u.v = _mm_cvtps_ph(x, _MM_FROUND_TO_NEAREST_INT); // VCVTPS2PH
    return u.c;
}

float hs_fastHalfToFloat(uint16_t c)
{
    union {
        __m128i v;
        uint16_t c;
    } u;
    u.c = c;
    __m128 w = _mm_cvtph_ps(u.v); // VCVTPH2PS
    float d;
    _mm_store_ss(&d, w);
    return d;
}

// Is this really faster than bit manipulation?
uint16_t hs_fastDoubleToHalf(double d)
{
    float f = (float)d;
    if ((double)f != d && isfinite(f)) {
        // The conversion was inexact.
        // Use "round-to-odd" trick.
        union {
            float x;
            struct {
                // little-endian
                unsigned mant: 23;
                unsigned exp: 8;
                unsigned sign: 1;
            };
        } w;
        w.x = f;
        w.mant |= 1;
        f = w.x;
    }
    __m128 x = _mm_set_ss(f);
    union {
        __m128i v;
        uint16_t c;
    } u;
    // A floating-point exception can be raised
    u.v = _mm_cvtps_ph(x, _MM_FROUND_TO_NEAREST_INT); // VCVTPS2PH
    return u.c;
}

double hs_fastHalfToDouble(uint16_t c)
{
    union {
        __m128i v;
        uint16_t c;
    } u;
    u.c = c;
    __m128 w = _mm_cvtph_ps(u.v); // VCVTPH2PS
    float d;
    _mm_store_ss(&d, w);
    return (double)d;
}

#else

// Let's hope _Float16 is available

uint16_t hs_fastFloatToHalf(float x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.f = (_Float16)x;
    return u.u;
}

float hs_fastHalfToFloat(uint16_t x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.u = x;
    return (float)u.f;
}

uint16_t hs_fastDoubleToHalf(double x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.f = (_Float16)x;
    return u.u;
}

double hs_fastHalfToDouble(uint16_t x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.u = x;
    return (double)u.f;
}

#endif
