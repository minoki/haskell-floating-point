{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Conversion
  ( realFloatToFrac
  , canonicalize
  , canonicalizeFloat
  , canonicalizeDouble
  ) where
import           GHC.Float.Compat (double2Float, float2Double)
import           MyPrelude

default ()

-- |
-- Converts a floating-point value into another type.
--
-- Similar to 'realToFrac', but treats NaN, infinities, negative zero even if the rewrite rule is off.
--
-- IEEE 754 @convertFormat@ operation.
realFloatToFrac :: (RealFloat a, Fractional b) => a -> b
realFloatToFrac x | isNaN x = 0/0
                  | isInfinite x = if x > 0 then 1/0 else -1/0
                  | isNegativeZero x = -0
                  | otherwise = realToFrac x
{-# NOINLINE [1] realFloatToFrac #-}
{-# RULES
"realFloatToFrac/a->a" realFloatToFrac = canonicalize
"realFloatToFrac/Float->Double" realFloatToFrac = float2Double
"realFloatToFrac/Double->Float" realFloatToFrac = double2Float
  #-}

-- Since GHC optimizes away '* 1.0' when the type is 'Float' or 'Double',
-- we can't canonicalize x by just 'x * 1.0'.
one :: Num a => a
one = 1
{-# NOINLINE one #-}

-- |
-- A specialized version of 'realFloatToFrac'.
--
-- The resulting value will be canonical and non-signaling.
canonicalize :: RealFloat a => a -> a
canonicalize x = x * one
{-# INLINE [1] canonicalize #-}

#if defined(HAS_FAST_CANONICALIZE)

foreign import ccall unsafe "hs_canonicalizeFloat"
  canonicalizeFloat :: Float -> Float
foreign import ccall unsafe "hs_canonicalizeDouble"
  canonicalizeDouble :: Double -> Double

{-# RULES
"canonicalize/Float" canonicalize = canonicalizeFloat
"canonicalize/Double" canonicalize = canonicalizeDouble
  #-}

#else

{-# SPECIALIZE canonicalize :: Float -> Float, Double -> Double #-}

canonicalizeFloat :: Float -> Float
canonicalizeFloat = canonicalize

canonicalizeDouble :: Double -> Double
canonicalizeDouble = canonicalize

#endif
