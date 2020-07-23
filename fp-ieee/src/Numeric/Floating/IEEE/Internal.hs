{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Numeric.Floating.IEEE.Internal
  ( module Numeric.Floating.IEEE.Internal
  , module Internal
  ) where
import           Numeric.Floating.IEEE.Internal.Augmented as Internal
import           Numeric.Floating.IEEE.Internal.Base as Internal hiding ((^!))
import           Numeric.Floating.IEEE.Internal.Classify as Internal
import           Numeric.Floating.IEEE.Internal.Conversion as Internal
import           Numeric.Floating.IEEE.Internal.FMA as Internal
import           Numeric.Floating.IEEE.Internal.GenericArith as Internal
import           Numeric.Floating.IEEE.Internal.MinMax as Internal
import           Numeric.Floating.IEEE.Internal.NextFloat as Internal
import           Numeric.Floating.IEEE.Internal.Remainder as Internal
import           Numeric.Floating.IEEE.Internal.Rounding as Internal
import           Numeric.Floating.IEEE.Internal.RoundToIntegral as Internal
#if defined(USE_HALF)
import           Numeric.Floating.IEEE.Internal.Half ()
#endif

-- |
-- IEEE 754 @convertFromInt@ operation, with each rounding attributes.
fromIntegerTiesToEven, fromIntegerTiesToAway, fromIntegerTowardPositive, fromIntegerTowardNegative, fromIntegerTowardZero :: RealFloat a => Integer -> a
fromIntegerTiesToEven = roundTiesToEven . fromIntegerR
fromIntegerTiesToAway = roundTiesToAway . fromIntegerR
fromIntegerTowardPositive = roundTowardPositive . fromIntegerR
fromIntegerTowardNegative = roundTowardNegative . fromIntegerR
fromIntegerTowardZero = roundTowardZero . fromIntegerR
{-# INLINE fromIntegerTiesToEven #-}
{-# INLINE fromIntegerTiesToAway #-}
{-# INLINE fromIntegerTowardPositive #-}
{-# INLINE fromIntegerTowardNegative #-}
{-# INLINE fromIntegerTowardZero #-}

fromIntegralTiesToEven, fromIntegralTiesToAway, fromIntegralTowardPositive, fromIntegralTowardNegative, fromIntegralTowardZero :: (Integral i, RealFloat a) => i -> a
fromIntegralTiesToEven = roundTiesToEven . fromIntegralR
fromIntegralTiesToAway = roundTiesToAway . fromIntegralR
fromIntegralTowardPositive = roundTowardPositive . fromIntegralR
fromIntegralTowardNegative = roundTowardNegative . fromIntegralR
fromIntegralTowardZero = roundTowardZero . fromIntegralR
{-# INLINE fromIntegralTiesToEven #-}
{-# INLINE fromIntegralTiesToAway #-}
{-# INLINE fromIntegralTowardPositive #-}
{-# INLINE fromIntegralTowardNegative #-}
{-# INLINE fromIntegralTowardZero #-}

fromRationalTiesToEven, fromRationalTiesToAway, fromRationalTowardPositive, fromRationalTowardNegative, fromRationalTowardZero :: RealFloat a => Rational -> a
fromRationalTiesToEven = roundTiesToEven . fromRationalR
fromRationalTiesToAway = roundTiesToAway . fromRationalR
fromRationalTowardPositive = roundTowardPositive . fromRationalR
fromRationalTowardNegative = roundTowardNegative . fromRationalR
fromRationalTowardZero = roundTowardZero . fromRationalR
{-# INLINE fromRationalTiesToEven #-}
{-# INLINE fromRationalTiesToAway #-}
{-# INLINE fromRationalTowardPositive #-}
{-# INLINE fromRationalTowardNegative #-}
{-# INLINE fromRationalTowardZero #-}

encodeFloatTiesToEven, encodeFloatTiesToAway, encodeFloatTowardPositive, encodeFloatTowardNegative, encodeFloatTowardZero :: RealFloat a => Integer -> Int -> a
encodeFloatTiesToEven m = roundTiesToEven . encodeFloatR m
encodeFloatTiesToAway m = roundTiesToAway . encodeFloatR m
encodeFloatTowardPositive m = roundTowardPositive . encodeFloatR m
encodeFloatTowardNegative m = roundTowardNegative . encodeFloatR m
encodeFloatTowardZero m = roundTowardZero . encodeFloatR m
{-# INLINE encodeFloatTiesToEven #-}
{-# INLINE encodeFloatTiesToAway #-}
{-# INLINE encodeFloatTowardPositive #-}
{-# INLINE encodeFloatTowardNegative #-}
{-# INLINE encodeFloatTowardZero #-}
