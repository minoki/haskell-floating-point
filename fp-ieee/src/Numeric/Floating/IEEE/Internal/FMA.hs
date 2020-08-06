{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.FMA
  ( isMantissaEven
  , twoSum
  , add_roundToOdd
  , split
  , twoProduct_generic
  , twoProductFloat_viaDouble
  , twoProduct
  , twoProduct_nonscaling
  , twoProductFloat
  , twoProductDouble
  , fusedMultiplyAdd_twoProduct
  , fusedMultiplyAddFloat_viaDouble
  , fusedMultiplyAdd_viaInteger
  , fusedMultiplyAdd_viaRational
  , fusedMultiplyAdd
  , fusedMultiplyAddFloat
  , fusedMultiplyAddDouble
  ) where
import           Control.Exception (assert)
import           Data.Bits
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                                   double2Float, float2Double)
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.Classify
import           Numeric.Floating.IEEE.Internal.NextFloat
import           Numeric.Floating.IEEE.Internal.Rounding (encodeFloatR, roundTiesToEven)

default ()

-- $setup
-- >>> :set -XScopedTypeVariables

-- Assumption: input is finite
isMantissaEven :: RealFloat a => a -> Bool
isMantissaEven 0 = True
isMantissaEven x = let !_ = assert (isFinite x) ()
                       (m,n) = decodeFloat x
                       d = floatDigits x
                       !_ = assert (floatRadix x ^ (d - 1) <= abs m && abs m < floatRadix x ^ d) ()
                       (expMin, _expMax) = floatRange x
                       s = expMin - (n + d)
                       !_ = assert (isDenormalized x == (s > 0)) ()
                   in if s > 0 then
                        even (m `shiftR` s)
                      else
                        even m
{-# NOINLINE [1] isMantissaEven #-}
{-# RULES
"isMantissaEven/Double"
  isMantissaEven = \x -> even (castDoubleToWord64 x)
"isMantissaEven/Float"
  isMantissaEven = \x -> even (castFloatToWord32 x)
  #-}

-- |
-- Returns @x := a + b@ and @x - \<the exact value of (a + b)\>@.
--
-- This function does not avoid undue overflow;
-- For example, the second component of
-- @twoSum (0x1.017bd555b0b1fp1022) (-0x1.fffffffffffffp1023)@
-- is a NaN.
--
-- prop> \(a :: Double) (b :: Double) -> let (_,expMax) = floatRange a in max (exponent a) (exponent b) < expMax ==> let (x, y) = twoSum a b in a + b == x && toRational a + toRational b == toRational x + toRational y
twoSum :: RealFloat a => a -> a -> (a, a)
twoSum a b =
  let x = a + b
      t = x - a
      y = (a - (x - t)) + (b - t)
      {-
        Alternative:
         y = if abs b <= abs a then
               b - (x - a)
             else
               a - (x - b)
      -}
  in (x, y)
{-# SPECIALIZE twoSum :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

-- |
-- Addition, with round to nearest odd floating-point number.
-- Like 'twoSum', this function does not handle undue overflow.
add_roundToOdd :: RealFloat a => a -> a -> a
add_roundToOdd x y = let (u, v) = twoSum x y
                         result | v < 0 && isMantissaEven u = nextDown u
                                | v > 0 && isMantissaEven u = nextUp u
                                | otherwise = u
                         !_ = assert (toRational u == toRational x + toRational y || not (isMantissaEven result)) ()
                     in result
{-# SPECIALIZE add_roundToOdd :: Float -> Float -> Float, Double -> Double -> Double #-}

-- This function doesn't handle overflow or underflow
split :: RealFloat a => a -> (a, a)
split a =
  let c = factor * a
      x = c - (c - a)
      y = a - x
  in (x, y)
  where factor = fromInteger $ 1 + floatRadix a ^! ((floatDigits a + 1) `quot` 2)
  -- factor == 134217729 for Double, 4097 for Float
{-# SPECIALIZE split :: Float -> (Float, Float), Double -> (Double, Double) #-}

twoProduct_generic :: RealFloat a => a -> a -> (a, a)
twoProduct_generic a b =
  let eab = exponent a + exponent b
      a' = significand a
      b' = significand b
      (ah, al) = split a'
      (bh, bl) = split b'
      x = a * b -- Since 'significand' doesn't honor the sign of zero, we can't use @a' * b'@
      y' = al * bl - (scaleFloat (-eab) x - ah * bh - al * bh - ah * bl)
  in (x, scaleFloat eab y')
{-# SPECIALIZE twoProduct_generic :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

twoProductFloat_viaDouble :: Float -> Float -> (Float, Float)
twoProductFloat_viaDouble a b =
  let x, y :: Float
      a', b', x' :: Double
      a' = float2Double a
      b' = float2Double b
      x' = a' * b'
      x = double2Float x'
      y = double2Float (x' - float2Double x)
  in (x, y)

-- This function will be rewritten into fastTwoProduct{Float,Double} if fast FMA is available; the rewriting may change behavior regarding overflow.
-- TODO: subnormal behavior?
-- |
-- prop> \(a :: Double) (b :: Double) -> let (x, y) = twoProduct a b in a * b == x && fromRational (toRational a * toRational b - toRational x) == y
twoProduct :: RealFloat a => a -> a -> (a, a)
twoProduct = twoProduct_generic
{-# INLINE [1] twoProduct #-}

-- This function will be rewritten into fastTwoProduct{Float,Double} if fast FMA is available; the rewriting may change behavior regarding overflow.
twoProduct_nonscaling :: RealFloat a => a -> a -> (a, a)
twoProduct_nonscaling a b =
  let (ah, al) = split a
      (bh, bl) = split b
      x = a * b
      y = al * bl - (x - ah * bh - al * bh - ah * bl)
  in (x, y)
{-# NOINLINE [1] twoProduct_nonscaling #-}

twoProductFloat :: Float -> Float -> (Float, Float)
twoProductDouble :: Double -> Double -> (Double, Double)

#if defined(HAS_FAST_FMA)

twoProductFloat x y = let !r = x * y
                          !s = fusedMultiplyAddFloat x y (-r)
                      in (r, s)

twoProductDouble x y = let !r = x * y
                           !s = fusedMultiplyAddDouble x y (-r)
                       in (r, s)

{-# RULES
"twoProduct/Float" twoProduct = twoProductFloat
"twoProduct/Double" twoProduct = twoProductDouble
"twoProduct_nonscaling/Float" twoProduct_nonscaling = twoProductFloat
"twoProduct_nonscaling/Double" twoProduct_nonscaling = twoProductDouble
  #-}

#else

twoProductFloat = twoProductFloat_viaDouble
{-# INLINE twoProductFloat #-}

twoProductDouble = twoProduct_generic

{-# RULES
"twoProduct/Float" twoProduct = twoProductFloat_viaDouble
"twoProduct_nonscaling/Float" twoProduct_nonscaling = twoProductFloat_viaDouble
  #-}
{-# SPECIALIZE twoProduct_nonscaling :: Double -> Double -> (Double, Double) #-}

#endif

fusedMultiplyAdd_twoProduct :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_twoProduct a b c
  | isFinite a && isFinite b && isFinite c =
    let eab | a == 0 || b == 0 = fst (floatRange a) - floatDigits a -- reasonably small
            | otherwise = exponent a + exponent b
        ec | c == 0 = fst (floatRange c) - floatDigits c
           | otherwise = exponent c

        -- Avoid overflow in twoProduct
        a' = significand a
        b' = significand b
        (x', y') = twoProduct_nonscaling a' b'
        !_ = assert (toRational a' * toRational b' == toRational x' + toRational y') ()

        -- Avoid overflow in twoSum
        e = max eab ec
        x = scaleFloat (eab - e) x'
        y = scaleFloat (eab - e) y'
        c'' = scaleFloat (max (fst (floatRange c) - floatDigits c + 1) (ec - e) - ec) c -- may be inexact

        (u1,u2) = twoSum y c''
        (v1,v2) = twoSum u1 x
        w = add_roundToOdd u2 v2
        result0 = v1 + w
        !_ = assert (result0 == fromRational (toRational x + toRational y + toRational c'')) ()
        result = scaleFloat e result0
        !_ = assert (result == fromRational (toRational a * toRational b + toRational c) || isDenormalized result) ()
    in if result0 == 0 then
         -- We need to handle the sign of zero
         if c == 0 && a /= 0 && b /= 0 then
           a * b -- let a * b underflow
         else
           a * b + c -- -0 if both a * b and c are -0
       else
         if isDenormalized result then
           -- The rounding in 'scaleFloat e result0' may yield an incorrect result.
           -- Take the slow path.
           case toRational a * toRational b + toRational c of
             0 -> a * b + c -- This should be exact
             r -> fromRational r
         else
           result
  | isFinite a && isFinite b = c -- c is +-Infinity or NaN
  | otherwise = a * b + c -- Infinity or NaN
{-# SPECIALIZE fusedMultiplyAdd_twoProduct :: Float -> Float -> Float -> Float, Double -> Double -> Double -> Double #-}

fusedMultiplyAddFloat_viaDouble :: Float -> Float -> Float -> Float
fusedMultiplyAddFloat_viaDouble a b c
  | isFinite a && isFinite b && isFinite c =
    let a', b', c' :: Double
        a' = float2Double a
        b' = float2Double b
        c' = float2Double c
        ab = a' * b' -- exact
        !_ = assert (toRational ab == toRational a' * toRational b') ()
        result = double2Float (add_roundToOdd ab c')
        !_ = assert (result == fromRational (toRational a * toRational b + toRational c)) ()
    in result
  | isFinite a && isFinite b = c -- a * b is finite, but c is Infinity or NaN
  | otherwise = a * b + c
  where
    !True = isFloatBinary32 || error "fusedMultiplyAdd/Float: Float must be IEEE binary32"
    !True = isDoubleBinary64 || error "fusedMultiplyAdd/Float: Double must be IEEE binary64"

fusedMultiplyAdd_viaInteger :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_viaInteger x y z
  | isFinite x && isFinite y && isFinite z =
      let (mx,ex) = decodeFloat x -- x == mx * b^ex, mx==0 || b^(d-1) <= abs mx < b^d
          (my,ey) = decodeFloat y -- y == my * b^ey, my==0 || b^(d-1) <= abs my < b^d
          (mz,ez) = decodeFloat z -- z == mz * b^ez, mz==0 || b^(d-1) <= abs mz < b^d
          exy = ex + ey
          ee = min ez exy
          !2 = floatRadix x
      in case mx * my `shiftL` (exy - ee) + mz `shiftL` (ez - ee) of
           0 -> x * y + z
           m -> roundTiesToEven (encodeFloatR m ee)
  | isFinite x && isFinite y = z -- x * y is finite, but z is Infinity or NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN
{-# NOINLINE [1] fusedMultiplyAdd_viaInteger #-}

fusedMultiplyAdd_viaRational :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_viaRational x y z
  | isFinite x && isFinite y && isFinite z =
      case toRational x * toRational y + toRational z of
        0 -> x * y + z
        r -> fromRational r
  | isFinite x && isFinite y = z -- x * is finite, but z is Infinity or NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN

-- |
-- IEEE 754 @fusedMultiplyAdd@ operation.
--
-- May make use of hardware FMA instructions if the target architecture has it; set @fma3@ package flag on x86 systems.
--
-- prop> \(a :: Double) (b :: Double) (c :: Double) -> fusedMultiplyAdd a b c == fromRational (toRational a * toRational b + toRational c)
fusedMultiplyAdd :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd = fusedMultiplyAdd_twoProduct
{-# INLINE [1] fusedMultiplyAdd #-}

#if defined(HAS_FAST_FMA)

foreign import ccall unsafe "hs_fusedMultiplyAddFloat"
  fusedMultiplyAddFloat :: Float -> Float -> Float -> Float
foreign import ccall unsafe "hs_fusedMultiplyAddDouble"
  fusedMultiplyAddDouble :: Double -> Double -> Double -> Double

{-# RULES
"fusedMultiplyAdd/Float" fusedMultiplyAdd = fusedMultiplyAddFloat
"fusedMultiplyAdd/Double" fusedMultiplyAdd = fusedMultiplyAddDouble
  #-}

#elif defined(USE_C99_FMA)

-- libm's fma might be implemented with hardware
foreign import ccall unsafe "fmaf"
  fusedMultiplyAddFloat :: Float -> Float -> Float -> Float
foreign import ccall unsafe "fma"
  fusedMultiplyAddDouble :: Double -> Double -> Double -> Double

{-# RULES
"fusedMultiplyAdd/Float" fusedMultiplyAdd = fusedMultiplyAddFloat
"fusedMultiplyAdd/Double" fusedMultiplyAdd = fusedMultiplyAddDouble
  #-}

#else

fusedMultiplyAddFloat :: Float -> Float -> Float -> Float
fusedMultiplyAddFloat = fusedMultiplyAddFloat_viaDouble
{-# INLINE fusedMultiplyAddFloat #-}

fusedMultiplyAddDouble :: Double -> Double -> Double -> Double
fusedMultiplyAddDouble = fusedMultiplyAdd_twoProduct
{-# INLINE fusedMultiplyAddDouble #-}

{-# RULES
"fusedMultiplyAdd/Float" fusedMultiplyAdd = fusedMultiplyAddFloat_viaDouble
"fusedMultiplyAdd/Double" fusedMultiplyAdd = fusedMultiplyAdd_twoProduct :: Double -> Double -> Double -> Double
  #-}

#endif
