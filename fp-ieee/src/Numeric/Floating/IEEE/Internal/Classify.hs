{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
module Numeric.Floating.IEEE.Internal.Classify where
import           Data.Bits
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                                   isDoubleFinite, isFloatFinite)
import           MyPrelude

default ()

-- |
-- IEEE 754 @isNormal@ operation.
isNormal :: RealFloat a => a -> Bool
isNormal x = x /= 0 && not (isNaN x) && not (isInfinite x) && not (isDenormalized x)
{-# NOINLINE [1] isNormal #-}
{-# RULES
"isNormal/Float" isNormal = isFloatNormal
"isNormal/Double" isNormal = isDoubleNormal
  #-}

isFloatNormal :: Float -> Bool
isFloatNormal x = let w = castFloatToWord32 x .&. 0x7f80_0000
                  in w /= 0 && w /= 0x7f80_0000

isDoubleNormal :: Double -> Bool
isDoubleNormal x = let w = castDoubleToWord64 x .&. 0x7ff0_0000_0000_0000
                   in w /= 0 && w /= 0x7ff0_0000_0000_0000

-- |
-- Returns @True@ if the argument is normal, subnormal, or zero.
--
-- IEEE 754 @isFinite@ operation.
isFinite :: RealFloat a => a -> Bool
isFinite x = not (isNaN x) && not (isInfinite x)
{-# NOINLINE [1] isFinite #-}
{-# RULES
"isFinite/Float"
  isFinite = \x -> isFloatFinite x /= 0
"isFinite/Double"
  isFinite = \x -> isDoubleFinite x /= 0
  #-}

-- |
-- Returns @True@ if the argument is zero.
--
-- IEEE 754 @isZero@ operation.
isZero :: RealFloat a => a -> Bool
isZero x = x == 0

-- |
-- Returns @True@ if the argument is negative (including negative zero).
--
-- Since 'RealFloat' constraint is insufficient to query the sign of NaNs,
-- this function treats all NaNs as positive.
-- See also "Numeric.Floating.IEEE.NaN".
--
-- IEEE 754 @isSignMinus@ operation.
isSignMinus :: RealFloat a => a -> Bool
isSignMinus x = x < 0 || isNegativeZero x

-- |
-- Comparison with IEEE 754 @totalOrder@ predicate.
--
-- Floating-point numbers are ordered as,
-- \(-\infty < \text{negative reals} < -0 < +0 < \text{positive reals} < +\infty < \mathrm{NaN}\).
--
-- Since 'RealFloat' constraint is insufficient to query the sign and payload of NaNs,
-- this function treats all NaNs as positive and does not make distinction between them.
-- See also "Numeric.Floating.IEEE.NaN".
--
-- Also, for the same reason, this function cannot distinguish the members of a cohort.
compareByTotalOrder :: RealFloat a => a -> a -> Ordering
compareByTotalOrder x y
  | x < y = LT
  | y < x = GT
  | x == y = if x == 0 then
               compare (isNegativeZero y) (isNegativeZero x)
             else
               EQ
  | otherwise = compare (isNaN x) (isNaN y) -- The sign bit and payload of NaNs are ignored
-- TODO: Specialize for Float, Double

-- |
-- Comparison with IEEE 754 @totalOrderMag@ predicate.
--
-- Equivalent to @'compareByTotalOrder' (abs x) (abs y)@.
compareByTotalOrderMag :: RealFloat a => a -> a -> Ordering
compareByTotalOrderMag x y = compareByTotalOrder (abs x) (abs y)

-- isCanonical :: a -> Bool

-- data PartialOrdering = LT | EQ | GT | UNORD

-- |
-- The classification of floating-point values.
data Class = SignalingNaN
           | QuietNaN
           | NegativeInfinity
           | NegativeNormal
           | NegativeSubnormal
           | NegativeZero
           | PositiveZero
           | PositiveSubnormal
           | PositiveNormal
           | PositiveInfinity
           deriving (Eq, Ord, Show, Read, Enum)

-- |
-- Classifies a floating-point value.
--
-- Since 'RealFloat' constraint is insufficient to query signaling status of a NaN, this function treats all NaNs as quiet.
-- See also "Numeric.Floating.IEEE.NaN".
classify :: RealFloat a => a -> Class
classify x | isNaN x                 = QuietNaN
           | x < 0, isInfinite x     = NegativeInfinity
           | x < 0, isDenormalized x = NegativeSubnormal
           | x < 0                   = NegativeNormal
           | isNegativeZero x        = NegativeZero
           | x == 0                  = PositiveZero
           | isDenormalized x        = PositiveSubnormal
           | isInfinite x            = PositiveInfinity
           | otherwise               = PositiveNormal
{-# NOINLINE [1] classify #-}
{-# RULES
"classify/Float" classify = classifyFloat
"classify/Double" classify = classifyDouble
  #-}

classifyFloat :: Float -> Class
classifyFloat x = let w = castFloatToWord32 x
                      s = testBit w 31 -- sign bit
                      e = (w `unsafeShiftR` 23) .&. 0xff -- exponent (8 bits)
                      m = w .&. 0x007f_ffff -- mantissa (23 bits without leading 1)
                   in case (s, e, m) of
                        (True,  0,    0) -> NegativeZero
                        (False, 0,    0) -> PositiveZero
                        (True,  0,    _) -> NegativeSubnormal
                        (False, 0,    _) -> PositiveSubnormal
                        (True,  0xff, 0) -> NegativeInfinity
                        (False, 0xff, 0) -> PositiveInfinity
                        (_,     0xff, _) -> QuietNaN -- treat all NaNs as quiet
                        (True,  _,    _) -> NegativeNormal
                        (False, _,    _) -> PositiveNormal

classifyDouble :: Double -> Class
classifyDouble x = let w = castDoubleToWord64 x
                       s = testBit w 63 -- sign bit
                       e = (w `unsafeShiftR` 52) .&. 0x7ff -- exponent (11 bits)
                       m = w .&. 0x000f_ffff_ffff_ffff -- mantissa (52 bits without leading 1)
                   in case (s, e, m) of
                        (True,  0,     0) -> NegativeZero
                        (False, 0,     0) -> PositiveZero
                        (True,  0,     _) -> NegativeSubnormal
                        (False, 0,     _) -> PositiveSubnormal
                        (True,  0x7ff, 0) -> NegativeInfinity
                        (False, 0x7ff, 0) -> PositiveInfinity
                        (_,     0x7ff, _) -> QuietNaN -- treat all NaNs as quiet
                        (True,  _,     _) -> NegativeNormal
                        (False, _,     _) -> PositiveNormal
