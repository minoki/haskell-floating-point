{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Rounded.Hardware.Internal.Conversion
  ( fromInt
  , intervalFromInteger_default
  , fromRatio
  , intervalFromRational_default
  ) where
import           Data.Functor.Product
import           Data.Ratio
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Numeric.Rounded.Hardware.Internal.Rounding

intervalFromInteger_default :: RealFloat a => Integer -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
intervalFromInteger_default x = case fromIntegerR x of Pair a b -> (Rounded (roundTowardNegative a), Rounded (roundTowardPositive b))
{-# SPECIALIZE intervalFromInteger_default :: Integer -> (Rounded 'TowardNegInf Float, Rounded 'TowardInf Float) #-}
{-# SPECIALIZE intervalFromInteger_default :: Integer -> (Rounded 'TowardNegInf Double, Rounded 'TowardInf Double) #-}

intervalFromRational_default :: RealFloat a => Rational -> (Rounded 'TowardNegInf a, Rounded 'TowardInf a)
intervalFromRational_default x = case fromRationalR x of Pair a b -> (Rounded (roundTowardNegative a), Rounded (roundTowardPositive b))
{-# SPECIALIZE intervalFromRational_default :: Rational -> (Rounded 'TowardNegInf Float, Rounded 'TowardInf Float) #-}
{-# SPECIALIZE intervalFromRational_default :: Rational -> (Rounded 'TowardNegInf Double, Rounded 'TowardInf Double) #-}

fromInt :: RealFloat a => RoundingMode -> Integer -> a
fromInt ToNearest    = fromIntegerTiesToEven
fromInt TowardZero   = fromIntegerTowardZero
fromInt TowardInf    = fromIntegerTowardPositive
fromInt TowardNegInf = fromIntegerTowardNegative
{-# SPECIALIZE fromInt :: RoundingMode -> Integer -> Float #-}
{-# SPECIALIZE fromInt :: RoundingMode -> Integer -> Double #-}

fromRatio :: (RealFloat a)
          => RoundingMode
          -> Integer -- ^ numerator
          -> Integer -- ^ denominator
          -> a
fromRatio ToNearest n d    = fromRationalTiesToEven (n % d)
fromRatio TowardZero n d   = fromRationalTowardZero (n % d)
fromRatio TowardInf n d    = fromRationalTowardPositive (n % d)
fromRatio TowardNegInf n d = fromRationalTowardNegative (n % d)
{-# SPECIALIZE fromRatio :: RoundingMode -> Integer -> Integer -> Float #-}
{-# SPECIALIZE fromRatio :: RoundingMode -> Integer -> Integer -> Double #-}
