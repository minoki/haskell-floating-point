{-# LANGUAGE DataKinds #-}
module Numeric.Rounded.Hardware.Internal.Conversion
  ( roundedFromInteger_default
  , roundedFromRational_default
  , intervalFromInteger_default
  , intervalFromRational_default
  ) where
import           Data.Functor.Product
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal (fromIntegerR, fromRationalR,
                                                 roundTowardNegative,
                                                 roundTowardPositive)
import           Numeric.Rounded.Hardware.Internal.Rounding

roundedFromInteger_default :: RealFloat a => RoundingMode -> Integer -> a
roundedFromInteger_default ToNearest    = fromIntegerTiesToEven
roundedFromInteger_default TowardZero   = fromIntegerTowardZero
roundedFromInteger_default TowardInf    = fromIntegerTowardPositive
roundedFromInteger_default TowardNegInf = fromIntegerTowardNegative
{-# INLINE roundedFromInteger_default #-}

roundedFromRational_default :: RealFloat a => RoundingMode -> Rational -> a
roundedFromRational_default ToNearest    = fromRationalTiesToEven
roundedFromRational_default TowardZero   = fromRationalTowardZero
roundedFromRational_default TowardInf    = fromRationalTowardPositive
roundedFromRational_default TowardNegInf = fromRationalTowardNegative
{-# INLINE roundedFromRational_default #-}

intervalFromInteger_default :: RealFloat a => Integer -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
intervalFromInteger_default x = case fromIntegerR x of
  Pair a b -> (Rounded (roundTowardNegative a), Rounded (roundTowardPositive b))
{-# INLINE intervalFromInteger_default #-}

intervalFromRational_default :: RealFloat a => Rational -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
intervalFromRational_default x = case fromRationalR x of
  Pair a b -> (Rounded (roundTowardNegative a), Rounded (roundTowardPositive b))
{-# INLINE intervalFromRational_default #-}
