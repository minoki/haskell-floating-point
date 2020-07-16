{-|
Module      : Numeric.Floating.Extra.IEEE
Description : IEEE 754-compliant operations for floating-point numbers

This module provides IEEE 754-compliant operations for floating-point numbers.

The functions in this module assume that the given floating-point type conform to IEEE 754 format.

Since 'RealFloat' constraint is insufficient to query properties of a NaN, the functions here assumes all NaN as positive, quiet.
If you want better treatment for NaNs, use the module "Numeric.Floating.Extra.IEEE.NaN".

Since floating-point exceptions cannot be accessed from Haskell in normal way, the operations provided by this module ignore exceptional behavior.
Don't let fp exceptions trap.

On i386 target, you may need to set @-msse2@ option to get correct floating-point behavior.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Floating.IEEE
  (
  -- * 5.3 Homogeneous general-computational operations
  --
  -- ** 5.3.1 General operations
    roundToIntegralTiesToEven
  , roundToIntegralTiesToAway
  , roundToIntegralTowardZero
  , roundToIntegralTowardPositive
  , roundToIntegralTowardNegative
  -- | @roundToIntegralExact{...}@: not implemented yet
  , nextUp
  , nextDown
  -- | 'nextTowardZero' is not in IEEE, but may be useful to some.
  , nextTowardZero -- not in IEEE
  , remainder

  -- ** 5.3.2 Decimal operations (not supported)
  --
  -- | Not supported.

  -- ** 5.3.3 logBFormat operations
  --
  -- | Not supported.
  , scaleFloat -- scaleB
  -- logB x = exponent x - 1

  -- * 5.4 formatOf general-computational operations
  --
  -- ** 5.4.1 Arithmetic operations
  --
  , (+) -- addition
  , (-) -- subtraction
  , (*) -- multiplication
  , (/) -- division
  , sqrt -- squareRoot
  , fusedMultiplyAdd
  , genericAdd
  , genericSub
  , genericMul
  , genericDiv
  -- | @genericSqrt@: not implemented yet
  , genericFusedMultiplyAdd
  -- |
  -- @convertFromInt@: not implemented yet
  , round    -- convertToIntegerTiesToEven: round
  , roundTiesToAway
  , truncate -- convertToIntegerTowardZero: truncate
  , ceiling  -- convertToIntegerTowardPositive: ceiling
  , floor    -- convertToIntegerTowardNegative: floor

  -- ** 5.4.2 Conversion operations for floating-point formats and decimal character sequences
  --
  , realFloatToFrac -- convertFormat
  -- |
  -- @convertFromDecimalCharacter@: not implemented. readSigned readFloat?
  --
  -- convertToDecimalCharacter: not implemented. show(E|F|G)Float?

  -- * 5.4.3 Conversion operations for binary formats
  --
  -- |
  -- convertFromHexCharacter: not implemented. readHexFloat?
  --
  -- convertToHexCharacter: not implemented. show(E|F|G)Float?

  -- * 5.5 Quiet-computational operations
  --
  -- ** 5.5.1 Sign bit operations
  -- copy: id
  , negate
  , abs
  -- | copySign: not implemented

  -- ** 5.5.2 Decimal re-encoding operations (not supported)
  --
  -- |
  -- Not supported.

  -- * 5.6 Signaling-computational operations
  --
  -- ** 5.6.1 Comparisons (not supported)
  --
  -- |
  -- This library does not support floating-point exceptions.

  -- * 5.7 Non-computational operations
  --
  -- ** 5.7.1 Conformance predicates (not supported)
  --
  -- |
  -- Not supported.

  -- ** 5.7.2 General operations
  , Class(..)
  , classify -- class
  , isSignMinus
  , isNormal
  , isFinite
  , isZero
  , isDenormalized -- isSubnormal
  , isInfinite -- re-export
  , isNaN -- re-export
  -- |
  -- isSignaling: not supported here.
  -- isCanonical: not supported.
  , floatRadix -- radix
  , compareByTotalOrder -- totalOrder
  , compareByTotalOrderMag -- totalOrderMag

  -- ** 5.7.3 Decimal operation (not supported)
  --
  -- |
  -- Not supported.

  -- ** 5.7.4 Operations on subsets of flags (not supported)
  --
  -- |
  -- Not supported.

  -- * 9. Recommended operations

  -- * 9.5 Augmented arithmetic operations
  , augmentedAddition
  , augmentedSubtraction
  , augmentedMultiplication

  -- * 9.6 Minimum and maximum operations
  , minimum'
  , minimumNumber
  , maximum'
  , maximumNumber
  , minimumMagnitude
  , minimumMagnitudeNumber
  , maximumMagnitude
  , maximumMagnitudeNumber

  -- * Uncategorized
  , minPositive
  , maxFinite
  , distanceUlp
  , twoSum
  , twoProduct
  , isMantissaEven
  , roundTiesTowardZero
  ) where
import           Data.Ratio
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Augmented
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.Classify
import           Numeric.Floating.IEEE.Internal.Conversion
import           Numeric.Floating.IEEE.Internal.FMA
import           Numeric.Floating.IEEE.Internal.GenericArith
import           Numeric.Floating.IEEE.Internal.MinMax
import           Numeric.Floating.IEEE.Internal.NextFloat
import           Numeric.Floating.IEEE.Internal.Round

distanceUlp :: RealFloat a => a -> a -> Maybe Integer
distanceUlp x y
  | isInfinite x || isInfinite y || isNaN x || isNaN y = Nothing
  | otherwise = let m = min (abs x) (abs y)
                    m' = nextUp m
                    v = (toRational y - toRational x) / toRational (m' - m)
                in if denominator v == 1
                   then Just (abs (numerator v))
                   else error "distanceUlp"

-- |
-- IEEE 754 @remainder@ operation.
remainder :: RealFloat a => a -> a -> a
remainder x y | isFinite x && isInfinite y = x
              | y == 0 || isInfinite y || isNaN y || not (isFinite x) = (x - x) / y * y -- return a NaN
              | otherwise = let n = round (toRational x / toRational y) -- TODO: Is round (x / y) okay?
                                r = x - y * fromInteger n
                            in r -- if r == 0, the sign of r is the same as x
{-# NOINLINE [1] remainder #-}

#ifdef USE_FFI
foreign import ccall unsafe "remainderf"
  c_remainderFloat :: Float -> Float -> Float
foreign import ccall unsafe "remainder"
  c_remainderDouble :: Double -> Double -> Double

{-# RULES
"remainder/Float" remainder = c_remainderFloat
"remainder/Double" remainder = c_remainderDouble
  #-}
#endif