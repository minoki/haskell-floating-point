/* This file was generated by etc/gen-rounded-common.sh. */

//
// double
//

static inline double rounded_add_impl_double(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double c = a + b;
    restore_fp_reg(oldreg);
    return c;
}
extern double rounded_hw_add_double(HsInt mode, double a, double b)
{ return rounded_add_impl_double(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_add_double_up(double a, double b)
{ return rounded_add_impl_double(ROUND_UPWARD, a, b); }
extern double rounded_hw_add_double_down(double a, double b)
{ return rounded_add_impl_double(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_add_double_zero(double a, double b)
{ return rounded_add_impl_double(ROUND_TOWARDZERO, a, b); }

static inline double rounded_sub_impl_double(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double c = a - b;
    restore_fp_reg(oldreg);
    return c;
}
extern double rounded_hw_sub_double(HsInt mode, double a, double b)
{ return rounded_sub_impl_double(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_sub_double_up(double a, double b)
{ return rounded_sub_impl_double(ROUND_UPWARD, a, b); }
extern double rounded_hw_sub_double_down(double a, double b)
{ return rounded_sub_impl_double(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_sub_double_zero(double a, double b)
{ return rounded_sub_impl_double(ROUND_TOWARDZERO, a, b); }

static inline double rounded_mul_impl_double(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double c = a * b;
    restore_fp_reg(oldreg);
    return c;
}
extern double rounded_hw_mul_double(HsInt mode, double a, double b)
{ return rounded_mul_impl_double(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_mul_double_up(double a, double b)
{ return rounded_mul_impl_double(ROUND_UPWARD, a, b); }
extern double rounded_hw_mul_double_down(double a, double b)
{ return rounded_mul_impl_double(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_mul_double_zero(double a, double b)
{ return rounded_mul_impl_double(ROUND_TOWARDZERO, a, b); }

static inline double rounded_div_impl_double(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double c = a / b;
    restore_fp_reg(oldreg);
    return c;
}
extern double rounded_hw_div_double(HsInt mode, double a, double b)
{ return rounded_div_impl_double(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_div_double_up(double a, double b)
{ return rounded_div_impl_double(ROUND_UPWARD, a, b); }
extern double rounded_hw_div_double_down(double a, double b)
{ return rounded_div_impl_double(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_div_double_zero(double a, double b)
{ return rounded_div_impl_double(ROUND_TOWARDZERO, a, b); }

static inline double rounded_sqrt_impl_double(native_rounding_mode mode, double a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double c = sqrt(a);
    restore_fp_reg(oldreg);
    return c;
}
extern double rounded_hw_sqrt_double(HsInt mode, double a)
{ return rounded_sqrt_impl_double(hs_rounding_mode_to_native(mode), a); }
extern double rounded_hw_sqrt_double_up(double a)
{ return rounded_sqrt_impl_double(ROUND_UPWARD, a); }
extern double rounded_hw_sqrt_double_down(double a)
{ return rounded_sqrt_impl_double(ROUND_DOWNWARD, a); }
extern double rounded_hw_sqrt_double_zero(double a)
{ return rounded_sqrt_impl_double(ROUND_TOWARDZERO, a); }

static inline double rounded_fma_impl_double(native_rounding_mode mode, double a, double b, double c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double result = fma(a, b, c);
    restore_fp_reg(oldreg);
    return result;
}
extern double rounded_hw_fma_double(HsInt mode, double a, double b, double c)
{ return rounded_fma_impl_double(hs_rounding_mode_to_native(mode), a, b, c); }
extern double rounded_hw_fma_double_up(double a, double b, double c)
{ return rounded_fma_impl_double(ROUND_UPWARD, a, b, c); }
extern double rounded_hw_fma_double_down(double a, double b, double c)
{ return rounded_fma_impl_double(ROUND_DOWNWARD, a, b, c); }
extern double rounded_hw_fma_double_zero(double a, double b, double c)
{ return rounded_fma_impl_double(ROUND_TOWARDZERO, a, b, c); }

static inline double rounded_fma_if_fast_impl_double(native_rounding_mode mode, double a, double b, double c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
#ifdef FP_FAST_FMA
    volatile double result = fma(a, b, c);
#else
    volatile double result = a * b + c;
#endif
    restore_fp_reg(oldreg);
    return result;
}
extern double rounded_hw_fma_if_fast_double(HsInt mode, double a, double b, double c)
{ return rounded_fma_if_fast_impl_double(hs_rounding_mode_to_native(mode), a, b, c); }
extern double rounded_hw_fma_if_fast_double_up(double a, double b, double c)
{ return rounded_fma_if_fast_impl_double(ROUND_UPWARD, a, b, c); }
extern double rounded_hw_fma_if_fast_double_down(double a, double b, double c)
{ return rounded_fma_if_fast_impl_double(ROUND_DOWNWARD, a, b, c); }
extern double rounded_hw_fma_if_fast_double_zero(double a, double b, double c)
{ return rounded_fma_if_fast_impl_double(ROUND_TOWARDZERO, a, b, c); }

//
// Conversion
//

static inline double rounded_int64_to_double_impl(native_rounding_mode mode, int64_t x)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double result = (double)x;
    restore_fp_reg(oldreg);
    return result;
}
extern double rounded_hw_int64_to_double(HsInt mode, int64_t x)
{ return rounded_int64_to_double_impl(hs_rounding_mode_to_native(mode), x); }
extern double rounded_hw_int64_to_double_up(int64_t x)
{ return rounded_int64_to_double_impl(ROUND_UPWARD, x); }
extern double rounded_hw_int64_to_double_down(int64_t x)
{ return rounded_int64_to_double_impl(ROUND_DOWNWARD, x); }
extern double rounded_hw_int64_to_double_zero(int64_t x)
{ return rounded_int64_to_double_impl(ROUND_TOWARDZERO, x); }

static inline double rounded_word64_to_double_impl(native_rounding_mode mode, uint64_t x)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile double result = (double)x;
    restore_fp_reg(oldreg);
    return result;
}
extern double rounded_hw_word64_to_double(HsInt mode, uint64_t x)
{ return rounded_word64_to_double_impl(hs_rounding_mode_to_native(mode), x); }
extern double rounded_hw_word64_to_double_up(uint64_t x)
{ return rounded_word64_to_double_impl(ROUND_UPWARD, x); }
extern double rounded_hw_word64_to_double_down(uint64_t x)
{ return rounded_word64_to_double_impl(ROUND_DOWNWARD, x); }
extern double rounded_hw_word64_to_double_zero(uint64_t x)
{ return rounded_word64_to_double_impl(ROUND_TOWARDZERO, x); }

//
// Interval arithmetic
//

static inline double fast_fmax_double(double x, double y)
{
    // should compile to MAX[SP][SD] instruction on x86
    return x > y ? x : y;
}
static inline double fast_fmax4_double(double x, double y, double z, double w)
{
    return fast_fmax_double(fast_fmax_double(x, y), fast_fmax_double(z, w));
}
static inline double fast_fmin_double(double x, double y)
{
    // should compile to MIN[SP][SD] instruction on x86
    return x < y ? x : y;
}
static inline double fast_fmin4_double(double x, double y, double z, double w)
{
    return fast_fmin_double(fast_fmin_double(x, y), fast_fmin_double(z, w));
}

extern double rounded_hw_interval_mul_double_up(double lo1, double hi1, double lo2, double hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    double x = (volatile double)(lo1 * lo2);
    double y = (volatile double)(lo1 * hi2);
    double z = (volatile double)(hi1 * lo2);
    double w = (volatile double)(hi1 * hi2);
    if (isnan(x)) x = 0.0; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 * inf -> 0 */
    double hi = fast_fmax4_double(x, y, z, w);
    restore_fp_reg(oldreg);
    return hi;
}

extern double rounded_hw_interval_mul_double_down(double lo1, double hi1, double lo2, double hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    double x = (volatile double)(lo1 * lo2);
    double y = (volatile double)(lo1 * hi2);
    double z = (volatile double)(hi1 * lo2);
    double w = (volatile double)(hi1 * hi2);
    if (isnan(x)) x = 0.0; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 * inf -> 0 */
    double lo = fast_fmin4_double(x, y, z, w);
    restore_fp_reg(oldreg);
    return lo;
}

extern double rounded_hw_interval_mul_add_double_up(double lo1, double hi1, double lo2, double hi2, double hi3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    double x = (volatile double)(lo1 * lo2);
    double y = (volatile double)(lo1 * hi2);
    double z = (volatile double)(hi1 * lo2);
    double w = (volatile double)(hi1 * hi2);
    if (isnan(x)) x = 0.0; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 * inf -> 0 */
    volatile double hi = fast_fmax4_double(x, y, z, w) + hi3;
    restore_fp_reg(oldreg);
    return hi;
}

extern double rounded_hw_interval_mul_add_double_down(double lo1, double hi1, double lo2, double hi2, double lo3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    double x = (volatile double)(lo1 * lo2);
    double y = (volatile double)(lo1 * hi2);
    double z = (volatile double)(hi1 * lo2);
    double w = (volatile double)(hi1 * hi2);
    if (isnan(x)) x = 0.0; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 * inf -> 0 */
    volatile double lo = fast_fmin4_double(x, y, z, w) + lo3;
    restore_fp_reg(oldreg);
    return lo;
}

extern double rounded_hw_interval_div_double_up(double lo1, double hi1, double lo2, double hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    double x = (volatile double)(lo1 / lo2);
    double y = (volatile double)(lo1 / hi2);
    double z = (volatile double)(hi1 / lo2);
    double w = (volatile double)(hi1 / hi2);
    if (isnan(x)) x = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    double hi = fast_fmax4_double(x, y, z, w);
    restore_fp_reg(oldreg);
    return hi;
}

extern double rounded_hw_interval_div_double_down(double lo1, double hi1, double lo2, double hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    double x = (volatile double)(lo1 / lo2);
    double y = (volatile double)(lo1 / hi2);
    double z = (volatile double)(hi1 / lo2);
    double w = (volatile double)(hi1 / hi2);
    if (isnan(x)) x = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    double lo = fast_fmin4_double(x, y, z, w);
    restore_fp_reg(oldreg);
    return lo;
}

extern double rounded_hw_interval_div_add_double_up(double lo1, double hi1, double lo2, double hi2, double hi3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    double x = (volatile double)(lo1 / lo2);
    double y = (volatile double)(lo1 / hi2);
    double z = (volatile double)(hi1 / lo2);
    double w = (volatile double)(hi1 / hi2);
    if (isnan(x)) x = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    volatile double hi = fast_fmax4_double(x, y, z, w) + hi3;
    restore_fp_reg(oldreg);
    return hi;
}

extern double rounded_hw_interval_div_add_double_down(double lo1, double hi1, double lo2, double hi2, double lo3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    double x = (volatile double)(lo1 / lo2);
    double y = (volatile double)(lo1 / hi2);
    double z = (volatile double)(hi1 / lo2);
    double w = (volatile double)(hi1 / hi2);
    if (isnan(x)) x = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0; /* 0 / 0, +-inf / +-inf -> 0 */
    volatile double lo = fast_fmin4_double(x, y, z, w) + lo3;
    restore_fp_reg(oldreg);
    return lo;
}

//
// Vector Operations
//

extern double rounded_hw_vector_sum_double(HsInt mode, HsInt length, HsInt offset, const double *a)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    volatile double s = 0.0;
    for (HsInt i = 0; i < length; ++i) {
        s += a[offset + i];
    }
    restore_fp_reg(oldreg);
    return s;
}

extern void rounded_hw_vector_add_double(HsInt mode, HsInt length, HsInt offsetR, double * restrict result, HsInt offsetA, const double * restrict a, HsInt offsetB, const double * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] + b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_sub_double(HsInt mode, HsInt length, HsInt offsetR, double * restrict result, HsInt offsetA, const double * restrict a, HsInt offsetB, const double * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] - b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_mul_double(HsInt mode, HsInt length, HsInt offsetR, double * restrict result, HsInt offsetA, const double * restrict a, HsInt offsetB, const double * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] * b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_fma_double(HsInt mode, HsInt length, HsInt offsetR, double * restrict result, HsInt offsetA, const double * restrict a, HsInt offsetB, const double * restrict b, HsInt offsetC, const double * restrict c)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = fma(a[offsetA + i], b[offsetB + i], c[offsetC + i]);
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_div_double(HsInt mode, HsInt length, HsInt offsetR, double * restrict result, HsInt offsetA, const double * restrict a, HsInt offsetB, const double * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] / b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_sqrt_double(HsInt mode, HsInt length, HsInt offsetR, double * restrict result, HsInt offsetA, const double * restrict a)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = sqrt(a[offsetA + i]);
    }
    restore_fp_reg(oldreg);
}

//
// float
//

static inline float rounded_add_impl_float(native_rounding_mode mode, float a, float b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float c = a + b;
    restore_fp_reg(oldreg);
    return c;
}
extern float rounded_hw_add_float(HsInt mode, float a, float b)
{ return rounded_add_impl_float(hs_rounding_mode_to_native(mode), a, b); }
extern float rounded_hw_add_float_up(float a, float b)
{ return rounded_add_impl_float(ROUND_UPWARD, a, b); }
extern float rounded_hw_add_float_down(float a, float b)
{ return rounded_add_impl_float(ROUND_DOWNWARD, a, b); }
extern float rounded_hw_add_float_zero(float a, float b)
{ return rounded_add_impl_float(ROUND_TOWARDZERO, a, b); }

static inline float rounded_sub_impl_float(native_rounding_mode mode, float a, float b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float c = a - b;
    restore_fp_reg(oldreg);
    return c;
}
extern float rounded_hw_sub_float(HsInt mode, float a, float b)
{ return rounded_sub_impl_float(hs_rounding_mode_to_native(mode), a, b); }
extern float rounded_hw_sub_float_up(float a, float b)
{ return rounded_sub_impl_float(ROUND_UPWARD, a, b); }
extern float rounded_hw_sub_float_down(float a, float b)
{ return rounded_sub_impl_float(ROUND_DOWNWARD, a, b); }
extern float rounded_hw_sub_float_zero(float a, float b)
{ return rounded_sub_impl_float(ROUND_TOWARDZERO, a, b); }

static inline float rounded_mul_impl_float(native_rounding_mode mode, float a, float b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float c = a * b;
    restore_fp_reg(oldreg);
    return c;
}
extern float rounded_hw_mul_float(HsInt mode, float a, float b)
{ return rounded_mul_impl_float(hs_rounding_mode_to_native(mode), a, b); }
extern float rounded_hw_mul_float_up(float a, float b)
{ return rounded_mul_impl_float(ROUND_UPWARD, a, b); }
extern float rounded_hw_mul_float_down(float a, float b)
{ return rounded_mul_impl_float(ROUND_DOWNWARD, a, b); }
extern float rounded_hw_mul_float_zero(float a, float b)
{ return rounded_mul_impl_float(ROUND_TOWARDZERO, a, b); }

static inline float rounded_div_impl_float(native_rounding_mode mode, float a, float b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float c = a / b;
    restore_fp_reg(oldreg);
    return c;
}
extern float rounded_hw_div_float(HsInt mode, float a, float b)
{ return rounded_div_impl_float(hs_rounding_mode_to_native(mode), a, b); }
extern float rounded_hw_div_float_up(float a, float b)
{ return rounded_div_impl_float(ROUND_UPWARD, a, b); }
extern float rounded_hw_div_float_down(float a, float b)
{ return rounded_div_impl_float(ROUND_DOWNWARD, a, b); }
extern float rounded_hw_div_float_zero(float a, float b)
{ return rounded_div_impl_float(ROUND_TOWARDZERO, a, b); }

static inline float rounded_sqrt_impl_float(native_rounding_mode mode, float a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float c = sqrtf(a);
    restore_fp_reg(oldreg);
    return c;
}
extern float rounded_hw_sqrt_float(HsInt mode, float a)
{ return rounded_sqrt_impl_float(hs_rounding_mode_to_native(mode), a); }
extern float rounded_hw_sqrt_float_up(float a)
{ return rounded_sqrt_impl_float(ROUND_UPWARD, a); }
extern float rounded_hw_sqrt_float_down(float a)
{ return rounded_sqrt_impl_float(ROUND_DOWNWARD, a); }
extern float rounded_hw_sqrt_float_zero(float a)
{ return rounded_sqrt_impl_float(ROUND_TOWARDZERO, a); }

static inline float rounded_fma_impl_float(native_rounding_mode mode, float a, float b, float c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float result = fmaf(a, b, c);
    restore_fp_reg(oldreg);
    return result;
}
extern float rounded_hw_fma_float(HsInt mode, float a, float b, float c)
{ return rounded_fma_impl_float(hs_rounding_mode_to_native(mode), a, b, c); }
extern float rounded_hw_fma_float_up(float a, float b, float c)
{ return rounded_fma_impl_float(ROUND_UPWARD, a, b, c); }
extern float rounded_hw_fma_float_down(float a, float b, float c)
{ return rounded_fma_impl_float(ROUND_DOWNWARD, a, b, c); }
extern float rounded_hw_fma_float_zero(float a, float b, float c)
{ return rounded_fma_impl_float(ROUND_TOWARDZERO, a, b, c); }

static inline float rounded_fma_if_fast_impl_float(native_rounding_mode mode, float a, float b, float c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
#ifdef FP_FAST_FMAF
    volatile float result = fmaf(a, b, c);
#else
    volatile float result = a * b + c;
#endif
    restore_fp_reg(oldreg);
    return result;
}
extern float rounded_hw_fma_if_fast_float(HsInt mode, float a, float b, float c)
{ return rounded_fma_if_fast_impl_float(hs_rounding_mode_to_native(mode), a, b, c); }
extern float rounded_hw_fma_if_fast_float_up(float a, float b, float c)
{ return rounded_fma_if_fast_impl_float(ROUND_UPWARD, a, b, c); }
extern float rounded_hw_fma_if_fast_float_down(float a, float b, float c)
{ return rounded_fma_if_fast_impl_float(ROUND_DOWNWARD, a, b, c); }
extern float rounded_hw_fma_if_fast_float_zero(float a, float b, float c)
{ return rounded_fma_if_fast_impl_float(ROUND_TOWARDZERO, a, b, c); }

//
// Conversion
//

static inline float rounded_int64_to_float_impl(native_rounding_mode mode, int64_t x)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float result = (float)x;
    restore_fp_reg(oldreg);
    return result;
}
extern float rounded_hw_int64_to_float(HsInt mode, int64_t x)
{ return rounded_int64_to_float_impl(hs_rounding_mode_to_native(mode), x); }
extern float rounded_hw_int64_to_float_up(int64_t x)
{ return rounded_int64_to_float_impl(ROUND_UPWARD, x); }
extern float rounded_hw_int64_to_float_down(int64_t x)
{ return rounded_int64_to_float_impl(ROUND_DOWNWARD, x); }
extern float rounded_hw_int64_to_float_zero(int64_t x)
{ return rounded_int64_to_float_impl(ROUND_TOWARDZERO, x); }

static inline float rounded_word64_to_float_impl(native_rounding_mode mode, uint64_t x)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    volatile float result = (float)x;
    restore_fp_reg(oldreg);
    return result;
}
extern float rounded_hw_word64_to_float(HsInt mode, uint64_t x)
{ return rounded_word64_to_float_impl(hs_rounding_mode_to_native(mode), x); }
extern float rounded_hw_word64_to_float_up(uint64_t x)
{ return rounded_word64_to_float_impl(ROUND_UPWARD, x); }
extern float rounded_hw_word64_to_float_down(uint64_t x)
{ return rounded_word64_to_float_impl(ROUND_DOWNWARD, x); }
extern float rounded_hw_word64_to_float_zero(uint64_t x)
{ return rounded_word64_to_float_impl(ROUND_TOWARDZERO, x); }

//
// Interval arithmetic
//

static inline float fast_fmax_float(float x, float y)
{
    // should compile to MAX[SP][SD] instruction on x86
    return x > y ? x : y;
}
static inline float fast_fmax4_float(float x, float y, float z, float w)
{
    return fast_fmax_float(fast_fmax_float(x, y), fast_fmax_float(z, w));
}
static inline float fast_fmin_float(float x, float y)
{
    // should compile to MIN[SP][SD] instruction on x86
    return x < y ? x : y;
}
static inline float fast_fmin4_float(float x, float y, float z, float w)
{
    return fast_fmin_float(fast_fmin_float(x, y), fast_fmin_float(z, w));
}

extern float rounded_hw_interval_mul_float_up(float lo1, float hi1, float lo2, float hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    float x = (volatile float)(lo1 * lo2);
    float y = (volatile float)(lo1 * hi2);
    float z = (volatile float)(hi1 * lo2);
    float w = (volatile float)(hi1 * hi2);
    if (isnan(x)) x = 0.0f; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 * inf -> 0 */
    float hi = fast_fmax4_float(x, y, z, w);
    restore_fp_reg(oldreg);
    return hi;
}

extern float rounded_hw_interval_mul_float_down(float lo1, float hi1, float lo2, float hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    float x = (volatile float)(lo1 * lo2);
    float y = (volatile float)(lo1 * hi2);
    float z = (volatile float)(hi1 * lo2);
    float w = (volatile float)(hi1 * hi2);
    if (isnan(x)) x = 0.0f; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 * inf -> 0 */
    float lo = fast_fmin4_float(x, y, z, w);
    restore_fp_reg(oldreg);
    return lo;
}

extern float rounded_hw_interval_mul_add_float_up(float lo1, float hi1, float lo2, float hi2, float hi3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    float x = (volatile float)(lo1 * lo2);
    float y = (volatile float)(lo1 * hi2);
    float z = (volatile float)(hi1 * lo2);
    float w = (volatile float)(hi1 * hi2);
    if (isnan(x)) x = 0.0f; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 * inf -> 0 */
    volatile float hi = fast_fmax4_float(x, y, z, w) + hi3;
    restore_fp_reg(oldreg);
    return hi;
}

extern float rounded_hw_interval_mul_add_float_down(float lo1, float hi1, float lo2, float hi2, float lo3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    float x = (volatile float)(lo1 * lo2);
    float y = (volatile float)(lo1 * hi2);
    float z = (volatile float)(hi1 * lo2);
    float w = (volatile float)(hi1 * hi2);
    if (isnan(x)) x = 0.0f; /* 0 * inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 * inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 * inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 * inf -> 0 */
    volatile float lo = fast_fmin4_float(x, y, z, w) + lo3;
    restore_fp_reg(oldreg);
    return lo;
}

extern float rounded_hw_interval_div_float_up(float lo1, float hi1, float lo2, float hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    float x = (volatile float)(lo1 / lo2);
    float y = (volatile float)(lo1 / hi2);
    float z = (volatile float)(hi1 / lo2);
    float w = (volatile float)(hi1 / hi2);
    if (isnan(x)) x = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    float hi = fast_fmax4_float(x, y, z, w);
    restore_fp_reg(oldreg);
    return hi;
}

extern float rounded_hw_interval_div_float_down(float lo1, float hi1, float lo2, float hi2)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    float x = (volatile float)(lo1 / lo2);
    float y = (volatile float)(lo1 / hi2);
    float z = (volatile float)(hi1 / lo2);
    float w = (volatile float)(hi1 / hi2);
    if (isnan(x)) x = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    float lo = fast_fmin4_float(x, y, z, w);
    restore_fp_reg(oldreg);
    return lo;
}

extern float rounded_hw_interval_div_add_float_up(float lo1, float hi1, float lo2, float hi2, float hi3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    float x = (volatile float)(lo1 / lo2);
    float y = (volatile float)(lo1 / hi2);
    float z = (volatile float)(hi1 / lo2);
    float w = (volatile float)(hi1 / hi2);
    if (isnan(x)) x = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    volatile float hi = fast_fmax4_float(x, y, z, w) + hi3;
    restore_fp_reg(oldreg);
    return hi;
}

extern float rounded_hw_interval_div_add_float_down(float lo1, float hi1, float lo2, float hi2, float lo3)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    float x = (volatile float)(lo1 / lo2);
    float y = (volatile float)(lo1 / hi2);
    float z = (volatile float)(hi1 / lo2);
    float w = (volatile float)(hi1 / hi2);
    if (isnan(x)) x = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(y)) y = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(z)) z = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    if (isnan(w)) w = 0.0f; /* 0 / 0, +-inf / +-inf -> 0 */
    volatile float lo = fast_fmin4_float(x, y, z, w) + lo3;
    restore_fp_reg(oldreg);
    return lo;
}

//
// Vector Operations
//

extern float rounded_hw_vector_sum_float(HsInt mode, HsInt length, HsInt offset, const float *a)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    volatile float s = 0.0f;
    for (HsInt i = 0; i < length; ++i) {
        s += a[offset + i];
    }
    restore_fp_reg(oldreg);
    return s;
}

extern void rounded_hw_vector_add_float(HsInt mode, HsInt length, HsInt offsetR, float * restrict result, HsInt offsetA, const float * restrict a, HsInt offsetB, const float * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] + b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_sub_float(HsInt mode, HsInt length, HsInt offsetR, float * restrict result, HsInt offsetA, const float * restrict a, HsInt offsetB, const float * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] - b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_mul_float(HsInt mode, HsInt length, HsInt offsetR, float * restrict result, HsInt offsetA, const float * restrict a, HsInt offsetB, const float * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] * b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_fma_float(HsInt mode, HsInt length, HsInt offsetR, float * restrict result, HsInt offsetA, const float * restrict a, HsInt offsetB, const float * restrict b, HsInt offsetC, const float * restrict c)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = fmaf(a[offsetA + i], b[offsetB + i], c[offsetC + i]);
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_div_float(HsInt mode, HsInt length, HsInt offsetR, float * restrict result, HsInt offsetA, const float * restrict a, HsInt offsetB, const float * restrict b)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = a[offsetA + i] / b[offsetB + i];
    }
    restore_fp_reg(oldreg);
}

extern void rounded_hw_vector_sqrt_float(HsInt mode, HsInt length, HsInt offsetR, float * restrict result, HsInt offsetA, const float * restrict a)
{
    native_rounding_mode nmode = hs_rounding_mode_to_native(mode);
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, nmode);
    for (HsInt i = 0; i < length; ++i) {
        result[offsetR + i] = sqrtf(a[offsetA + i]);
    }
    restore_fp_reg(oldreg);
}