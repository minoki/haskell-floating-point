#include <stdint.h> // uint16_t
#include <string.h>
#include <math.h>

/*
 * binary16
 *   p = 11
 *   emin = -14
 *   emax = 15
 *   floatRange _ = (-13,16)
 *   minPositive = 0x1p-24
 *   minPositiveNormal = 0x1p-14
 *   maxFinite = 0x1.ffcp15
 *   exponentBias = 15
 */

/*
 * binary32
 *   p = 24
 *   emin = -126
 *   emax = 127
 *   floatRange _ = (-125,128)
 *   minPositive = 0x1p-149
 *   minPositiveNormal = 0x1p-126
 *   maxFinite = 0x1.fffffep127
 *   exponentBias = 127
 */

/*
 * binary64
 *   p = 53
 *   emin = -1022
 *   emax = 1023
 *   floatRange _ = (-1021,1024)
 *   minPositive = 0x1p-1074
 *   minPositiveNormal = 0x1p-1022
 *   maxFinite = 0x1.ffff_ffff_ffff_fp1023
 *   exponentBias = 1023
 */

// TODO: Use AVX-512 FP16 if available

#if defined(__F16C__) // x86 F16C

#include <x86intrin.h>

uint16_t hs_fp_ieee_floatToHalf(float f)
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

float hs_fp_ieee_halfToFloat(uint16_t c)
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

uint16_t hs_fp_ieee_doubleToHalf(double d)
{
    uint64_t q;
    memcpy(&q, &d, sizeof(d));
    const uint64_t F64_SIGN_MASK   = UINT64_C(0x8000000000000000);
    const uint64_t F64_EXP_MASK    = UINT64_C(0x7ff0000000000000);
    const uint64_t F64_MANT_MASK   = UINT64_C(0x000fffffffffffff);
    const uint64_t UPPER_MANT_MASK = UINT64_C(0x000ffc0000000000);
    const uint64_t LOWER_MANT_MASK = UINT64_C(0x000003ffffffffff);
    const uint64_t TIE             = UINT64_C(0x0000020000000000);
    uint16_t f16sign = (uint16_t)((q & F64_SIGN_MASK) >> (63 - 15));
    uint64_t biasedExp = (q & F64_EXP_MASK) >> 52;
    //        biasedExp - 1023 <= -26: round to zero
    //        biasedExp - 1023 == -25: if abs d == 0x1p-25 then +-0 else +-0x1p-24
    // -24 <= biasedExp - 1023 <= -16: subnormal
    //        biasedExp - 1023 == -15: if abs d >= 0x1.ffep-15 then +-0x1p-14 else subnormal
    // -14 <= biasedExp - 1023 <=  14: normal
    //        biasedExp - 1023 ==  15: if abs d >= 0x1.ffep15 then +-infinity else normal
    //  16 <= biasedExp - 1023       : infinity
    if (biasedExp < 1023 - 25) {
        // biasedExp - 1023 < -25: zero
        return f16sign | 0;
    } else if (biasedExp < 1023 - 14) {
        // biasedExp - 1023 < -14: zero / subnormal / minPositiveNormal
        // 2^(biasedExp - 1023) <= abs d < 2^(biasedExp - 1022)
        // ulp = 0x1p-24
        // 0b1XX..X * 0x1p-24
        //   ^^   ^
        //   ||   +- 2^(-24)
        //   | \         : (biasedExp - 1024) - (-24) + 1 = biasedExp - (1024 - 24 - 1)
        //    \ ---- 2^(biasedExp - 1024)
        //     ----- 2^(biasedExp - 1023)
        int bitWidth = biasedExp - (1024 - 24 - 1);
        uint64_t mant = (F64_MANT_MASK + 1) | (q & F64_MANT_MASK);
        uint16_t f16mant = (uint16_t)(mant >> (52 - bitWidth));
        uint64_t lower = mant & (((LOWER_MANT_MASK + 1) << (10 - bitWidth)) - 1);
        uint64_t tie = TIE << (10 - bitWidth);
        if (lower < tie || (lower == tie && (f16mant & 1) == 0)) {
            return f16sign | f16mant;
        } else {
            return (f16sign | f16mant) + 1; // may be +-minPositiveNormal
        }
    } else if (biasedExp < 1023 + 16) {
        // biasedExp - 1023 < 16: normal / infinity
        uint16_t f16mant = (uint16_t)((q & UPPER_MANT_MASK) >> (53 - 11));
        uint16_t f16exp = (uint16_t)((biasedExp - (1023 - 15)) << 10);
        uint64_t lower = q & LOWER_MANT_MASK;
        if (lower < TIE || (lower == TIE && (f16mant & 1) == 0)) {
            return f16sign | f16exp | f16mant;
        } else {
            return (f16sign | f16exp | f16mant) + 1; // may be +-infinity
        }
    } else {
        // infinity or NaN
        if (isnan(d)) {
            // keep the sign bit
            // discard the payload
            return f16sign | 0x7e00;
        } else {
            return f16sign | 0x7c00;
        }
    }
}

double hs_fp_ieee_halfToDouble(uint16_t c)
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

uint16_t hs_fp_ieee_floatToHalf(float x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.f = (_Float16)x;
    return u.u;
}

float hs_fp_ieee_halfToFloat(uint16_t x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.u = x;
    return (float)u.f;
}

uint16_t hs_fp_ieee_doubleToHalf(double x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.f = (_Float16)x;
    return u.u;
}

double hs_fp_ieee_halfToDouble(uint16_t x)
{
    union {
        _Float16 f;
        uint16_t u;
    } u;
    u.u = x;
    return (double)u.f;
}

#endif
