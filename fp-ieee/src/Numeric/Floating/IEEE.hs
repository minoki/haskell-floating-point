{-|
Module      : Numeric.Floating.IEEE
Description : IEEE 754-compliant operations for floating-point numbers

This module provides IEEE 754-compliant operations for floating-point numbers.

The functions in this module assume that the given floating-point type conform to IEEE 754 format.

Since 'RealFloat' constraint is insufficient to query properties of a NaN, the functions here assumes all NaN as positive, quiet.
If you want better treatment for NaNs, use the module "Numeric.Floating.IEEE.NaN".

Since floating-point exceptions cannot be accessed from Haskell, the operations provided by this module ignore exceptional behavior.
This library assumes the default exception handling is in use.

If you are using GHC <= 8.8 on i386 target, you may need to set @-msse2@ option to get correct floating-point behavior.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE
  (
  -- * Standard Haskell classes
  --
  -- $stdclasses

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
  , nextTowardZero -- not in IEEE
  , remainder

  -- ** 5.3.2 Decimal operations (not supported)
  --
  -- | Not supported.

  -- ** 5.3.3 logBFormat operations
  --
  , scaleFloatTiesToEven
  , scaleFloatTiesToAway
  , scaleFloatTowardPositive
  , scaleFloatTowardNegative
  , scaleFloatTowardZero
  -- |
  -- The Haskell counterpart for IEEE 754 @logB@ operation is 'exponent'.
  -- Note that @logB@ and 'exponent' are different by one:
  -- @logB x = 'exponent' x - 1@
  , exponent

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
  -- | @genericSqrt@ is not implemented yet.
  , genericFusedMultiplyAdd
  , fromIntegerTiesToEven
  , fromIntegerTiesToAway
  , fromIntegerTowardPositive
  , fromIntegerTowardNegative
  , fromIntegerTowardZero
  , fromIntegralTiesToEven
  , fromIntegralTiesToAway
  , fromIntegralTowardPositive
  , fromIntegralTowardNegative
  , fromIntegralTowardZero
  , fromRationalTiesToEven
  , fromRationalTiesToAway
  , fromRationalTowardPositive
  , fromRationalTowardNegative
  , fromRationalTowardZero
  , round     -- convertToIntegerTiesToEven
  , roundAway -- convertToIntegerTiesToAway
  , truncate  -- convertToIntegerTowardZero
  , ceiling   -- convertToIntegerTowardPositive
  , floor     -- convertToIntegerTowardNegative

  -- ** 5.4.2 Conversion operations for floating-point formats and decimal character sequences
  --
  -- |
  -- Unfortunately, 'realToFrac' does not have a good semantics, and behaves differently with rewrite rules (consider @realToFrac (0/0 :: Float) :: Double@).
  -- As an alternative, this library provides 'realFloatToFrac', with well-defined semantics on signed zeroes, infinities and NaNs.
  -- Like 'realToFrac', 'realFloatToFrac' comes with some rewrite rules for particular types, but they should not change behavior.
  , realFloatToFrac -- convertFormat
  , canonicalize
  -- |
  -- @convertFromDecimalCharacter@: not implemented.
  --
  -- @convertToDecimalCharacter@: not implemented.

  -- * 5.4.3 Conversion operations for binary formats
  --
  -- |
  -- @convertFromHexCharacter@: not implemented.
  --
  -- @convertToHexCharacter@: 'Numeric.showHFloat' from "Numeric" can be used.

  -- * 5.5 Quiet-computational operations
  --
  -- ** 5.5.1 Sign bit operations
  --
  -- |
  -- For IEEE-compliant floating-point types, 'negate' from "Prelude" should comply with IEEE semantics.
  -- 'abs' should also comply with IEEE semantics, but unfortunately it does not handle the sign bit of NaN on via-C backend and SPARC NCG backend.
  , negate
  , abs
  -- |
  -- See "Numeric.Floating.IEEE.NaN" for @copySign@.

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
  -- See "Numeric.Floating.IEEE.NaN" for @isSignaling@.
  --
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

-- $stdclasses
--
-- This library assumes that some of the standard numeric functions correspond to the operations specified by IEEE.
-- The rounding attribute should be roundTiesToEven and the exceptional behavior should be the default one.
--
-- == 'Num'
--
--     * '(+)', '(-)', and '(*)' should be correctly-rounding.
--     * 'negate' should comply with IEEE semantics.
--     * 'abs' should comply with IEEE semantics, but unfortunately it does not handle the sign bit of NaN for 'Float' and 'Double' on via-C backend and SPARC NCG backend.
--     * 'fromInteger' should be correctly-rounding, but unfortunately not for 'Float' and 'Double' on GHC < 9.2 (see GHC's [#17231](https://gitlab.haskell.org/ghc/ghc/-/issues/17231)).
--       This module provides an always-correctly-rounding alternative: 'fromIntegerTiesToEven'.
--
-- == 'Fractional'
--
--     * '(/)' should be correctly-rounding.
--     * 'fromRational' should be correctly-rounding, but some third-partiy floating-point types fail to do so.
--
-- == 'Floating'
--
--     * 'sqrt' should be correctly-rounding.
--
-- == 'RealFrac'
--
--     * 'truncate': IEEE 754 @convertToIntegerTowardZero@ operation.
--     * 'round': IEEE 754 @convertToIntegerTiesToEven@ operation; the Language Report says that this should choose the even integer if the argument is the midpoint of two successive integers.
--     * 'ceiling': IEEE 754 @convertToIntegerTowardPositive@ operation.
--     * 'floor': IEEE 754 @convertToIntegerTowardNegative@ operation.
--
-- To complete these, 'roundAway' is provided by this library.
-- Note that Haskell's 'round' is specified to be ties-to-even, whereas C's @round@ is ties-to-away.
--
-- == 'RealFloat'
--
-- This class provides information on the IEEE-compliant format.
--
--     * 'floatRadix': The base \(b\). IEEE 754 @radix@ operation.
--     * 'floatDigits': The precision \(p\).
--     * 'floatRange': The exponent range offset by 1: \((\mathit{emin}+1,\mathit{emax}+1)\)
--     * @'decodeFloat' x@: The exponent part returned is in the range \([\mathit{emin}+1-p,\mathit{emax}+1-p]\) if @x@ is normal, or in \([\mathit{emin}-2p+2,\mathit{emin}-p]\) if @x@ is subnormal.
--     * 'encodeFloat' should accept the significand in the range @[0, floatRadix x ^ floatDigits x]@. This library does not assume a particular rounding behavior when the result cannot be expressed in the target type.
--     * @'exponent' x@: The exponent offset by 1: \(\mathrm{logB}(x)+1\). Returns an integer in \([\mathit{emin}+1,\mathit{emax}+1]\) if @x@ is normal, or in \([\mathit{emin}-p+2,\mathit{emin}]\) if @x@ is subnormal.
--     * @'significand' x@: Returns the significand of @x@ as a value between \([1/b,1)\).
--     * 'scaleFloat': This library does not assume a particular rounding behavior when the result is subnormal.
--     * 'isNaN'
--     * 'isInfinite'
--     * 'isDenormalized'
--     * 'isNegativeZero'
--     * 'isIEEE' should return @True@ if you are using the type with this library.
