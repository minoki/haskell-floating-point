{-|
Module      : Numeric.Floating.Extra.IEEE
Description : IEEE 754-compliant operations for floating-point numbers

This module provides IEEE 754-compliant operations for floating-point numbers.

The functions in this module assume that the given floating-point type conform to IEEE 754 format.

Since 'RealFloat' constraint is insufficient to query properties of a NaN, the functions here assumes all NaN as positive, quiet.
If you want better treatment for NaNs, use the module "Numeric.Floating.IEEE.NaN".

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
    round'
  , roundAway'
  , truncate'
  , ceiling'
  , floor'
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
  -- |
  -- For IEEE-compliant floating-point types, '(+)', '(-)', '(*)', '(/)', and 'sqrt' from "Prelude" should be correctly-rounding.
  -- 'fusedMultiplyAdd' is provided by this library.
  -- This library also provides \"generic\" version of the arithmetic operations, which can be useful if the target type is narrower than source.
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
  -- The rounding direction of 'fromInteger' for the standard floating-point types may vary depending on the magnitude of input.
  -- To get consistend rounding behavior, 'fromIntegerTiesToEven' can be used instead.
  , fromIntegerTiesToEven
  , fromIntegerTiesToAway
  , fromIntegerTowardPositive
  , fromIntegerTowardNegative
  , fromIntegerTowardZero
  -- |
  -- For IEEE-compliant floating-point types, 'round', 'truncate', 'ceiling', and 'floor' from 'RealFrac" class should be correct implementation of IEEE 754 @convertToInteger@ operations.
  -- To complete them, 'roundAway' is provided by this library.
  -- Note that Haskell's 'round' is specified to be ties-to-even, whereas C's @round@ is ties-to-away.
  , round    -- convertToIntegerTiesToEven: round
  , roundAway
  , truncate -- convertToIntegerTowardZero: truncate
  , ceiling  -- convertToIntegerTowardPositive: ceiling
  , floor    -- convertToIntegerTowardNegative: floor

  -- ** 5.4.2 Conversion operations for floating-point formats and decimal character sequences
  --
  -- |
  -- Unfortunately, the good old 'realToFrac' does not have a good semantics, and changes behavior with rewrite rules (consider @realToFrac (0/0 :: Float) :: Double@).
  -- This library provides 'realFloatToFrac', with good semantics on signed zeroes, infinities and NaNs.
  -- Like 'realToFrac', 'realFloatToFrac' comes with some rewrite rules for particular types, but they should not change behavior.
  , realFloatToFrac -- convertFormat
  -- |
  -- @convertFromDecimalCharacter@: not implemented.
  --
  -- convertToDecimalCharacter: not implemented.

  -- * 5.4.3 Conversion operations for binary formats
  --
  -- |
  -- convertFromHexCharacter: not implemented.
  --
  -- convertToHexCharacter: not implemented.

  -- * 5.5 Quiet-computational operations
  --
  -- ** 5.5.1 Sign bit operations
  --
  -- |
  -- For IEEE-compliant floating-point types, 'negate' and 'abs' from "Prelude" should comply with IEEE semantics.
  , negate
  , abs
  -- |
  -- @copySign@: not implemented

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
  --
  -- |
  -- Functions in this module disregards the content of NaNs: sign bit, signaling-or-quiet, and payload.
  -- All NaNs are treated as quiet, positive.
  -- To properly handle NaNs, use the typeclass and functions from "Numeric.Floating.IEEE.NaN".
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
  -- @isSignaling@: not supported here.
  -- @isCanonical@: not supported.
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

  -- * Floating-point constants
  , minPositive
  , minPositiveNormal
  , maxFinite
  ) where
import           MyPrelude
import           Numeric.Floating.IEEE.Internal
