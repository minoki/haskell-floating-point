{-# LANGUAGE NumericUnderscores #-}
module Numeric.Floating.IEEE.Internal.NaN
  ( module Numeric.Floating.IEEE.Internal.NaN
  , Class (..)
  ) where
import           Data.Bits
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                                   castWord32ToFloat, castWord64ToDouble)
import           Numeric.Floating.IEEE.Internal.Classify (Class (..))

-- | An instance of this class supports manipulation of NaN.
class RealFloat a => RealFloatNaN a where
  {-# MINIMAL (copySign | isSignMinus), (isSignaling | classify), getPayload, setPayload, setPayloadSignaling #-}

  -- 5.5.1 Sign bit operations
  -- |
  -- Returns the first operand, with the sign of the second.
  --
  -- IEEE 754 @copySign@ operation.
  copySign :: a -> a -> a
  copySign x y = if isSignMinus x == isSignMinus y then
                   x
                 else
                   -x

  -- 5.7.2 General operations
  -- |
  -- Returns @True@ if the operand is a negative number, negative infinity, negative zero, or a NaN with negative sign bit.
  --
  -- IEEE 754 @isSignMinus@ operation.
  isSignMinus :: a -> Bool
  isSignMinus x = copySign 1.0 x < 0

  -- |
  -- Returns @True@ if the operand is a signaling NaN.
  --
  -- IEEE 754 @isSignaling@ operation.
  --
  -- Warning: GHC's optimizer is not aware of signaling NaNs.
  isSignaling :: a -> Bool
  isSignaling x = classify x == SignalingNaN

  -- 9.7 NaN payload operations

  -- |
  -- Returns the payload of a NaN.
  -- Returns @-1@ if the operand is not a NaN.
  --
  -- IEEE 754 @getPayload@ operation.
  getPayload :: a -> a

  -- |
  -- Returns a quiet NaN with a given payload.
  -- Returns a positive zero if the payload is invalid.
  --
  -- IEEE 754 @setPayload@ operation.
  setPayload :: a -> a

  -- |
  -- Returns a signaling NaN with a given payload.
  -- Returns a positive zero if the payload is invalid.
  --
  -- IEEE 754 @setPayloadSignaling@ operation.
  setPayloadSignaling :: a -> a

  -- |
  -- IEEE 754 @class@ operation.
  classify :: a -> Class
  classify = classifyDefault

  -- |
  -- Equality with IEEE 754 @totalOrder@ operation.
  equalByTotalOrder :: a -> a -> Bool
  equalByTotalOrder x y = compareByTotalOrder x y == EQ

  -- |
  -- Comparison with IEEE 754 @totalOrder@ operation.
  --
  -- Floating-point numbers should be ordered as,
  -- \(-\mathrm{qNaN} < -\mathrm{sNaN} < -\infty < \text{negative reals} < -0 < +0 < \text{positive reals} < +\infty < +\mathrm{sNaN} < +\mathrm{qNaN}\).
  compareByTotalOrder :: a -> a -> Ordering
  compareByTotalOrder = compareByTotalOrderDefault

classifyDefault :: RealFloatNaN a => a -> Class
classifyDefault x
  | isNaN x                 = if isSignaling x then
                                SignalingNaN
                              else
                                QuietNaN
  | x < 0, isInfinite x     = NegativeInfinity
  | x < 0, isDenormalized x = NegativeSubnormal
  | x < 0                   = NegativeNormal
  | isNegativeZero x        = NegativeZero
  | x == 0                  = PositiveZero
  | isDenormalized x        = PositiveSubnormal
  | isInfinite x            = PositiveInfinity
  | otherwise               = PositiveNormal

compareByTotalOrderDefault :: RealFloatNaN a => a -> a -> Ordering
compareByTotalOrderDefault x y
  | x < y = LT
  | y < x = GT
  | x == y = if x == 0 then
               compare (isNegativeZero y) (isNegativeZero x)
             else
               EQ -- TODO: cohort?
  | otherwise = compare (isSignMinus y) (isSignMinus x)
                <> let r = compare (isNaN x) (isNaN y) -- number < +NaN
                           <> compare (isSignaling y) (isSignaling x) -- +(signaling NaN) < +(quiet NaN)
                           <> compare (getPayload x) (getPayload y) -- implementation-defined
                   in if isSignMinus x then
                        compare EQ r
                      else
                        r

instance RealFloatNaN Float where
  copySign x y = castWord32ToFloat ((x' .&. 0x7fff_ffff) .|. (y' .&. 0x8000_0000))
    where x' = castFloatToWord32 x
          y' = castFloatToWord32 y

  isSignMinus x = testBit (castFloatToWord32 x) 31

  isSignaling x = x' .&. 0x7f80_0000 == 0x7f80_0000 && x' .&. 0x7fff_ffff /= 0x7f80_0000 && not (testBit x' 22)
    where x' = castFloatToWord32 x

  getPayload x
    | not (isNaN x) = -1
    | otherwise = fromIntegral (castFloatToWord32 x .&. 0x007f_ffff)

  setPayload x
    | 0 <= x && x <= 0x007f_ffff = castWord32ToFloat $ round x .|. 0x7fc0_0000
    | otherwise = 0

  setPayloadSignaling x
    | 0 < x && x <= 0x007f_ffff = castWord32ToFloat $ round x .|. 0x7f80_0000
    | otherwise = 0

  classify x = let w = castFloatToWord32 x
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
                    (_,     0xff, _) -> if testBit w 22 then
                                          QuietNaN
                                        else
                                          SignalingNaN
                    (True,  _,    _) -> NegativeNormal
                    (False, _,    _) -> PositiveNormal

  equalByTotalOrder x y = castFloatToWord32 x == castFloatToWord32 y

  compareByTotalOrder x y = let x' = castFloatToWord32 x
                                y' = castFloatToWord32 y
                            in compare (testBit y' 31) (testBit x' 31) -- sign bit
                               <> if testBit x' 31 then
                                    compare y' x' -- negative
                                  else
                                    compare x' y' -- positive

instance RealFloatNaN Double where
  copySign x y = castWord64ToDouble ((x' .&. 0x7fff_ffff_ffff_ffff) .|. (y' .&. 0x8000_0000_0000_0000))
    where x' = castDoubleToWord64 x
          y' = castDoubleToWord64 y

  isSignMinus x = testBit (castDoubleToWord64 x) 63

  isSignaling x = x' .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000 && x' .&. 0x7fff_ffff_ffff_ffff /= 0x7ff0_0000_0000_0000 && not (testBit x' 51)
    where x' = castDoubleToWord64 x

  getPayload x
    | not (isNaN x) = -1
    | otherwise = fromIntegral (castDoubleToWord64 x .&. 0x0007_ffff_ffff_ffff)

  setPayload x
    | 0 <= x && x <= 0x0007_ffff_ffff_ffff = castWord64ToDouble $ round x .|. 0x7ff8_0000_0000_0000
    | otherwise = 0

  setPayloadSignaling x
    | 0 < x && x <= 0x0007_ffff_ffff_ffff = castWord64ToDouble $ round x .|. 0x7ff0_0000_0000_0000
    | otherwise = 0

  classify x = let w = castDoubleToWord64 x
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
                    (_,     0x7ff, _) -> if testBit w 51 then
                                           QuietNaN
                                         else
                                           SignalingNaN
                    (True,  _,     _) -> NegativeNormal
                    (False, _,     _) -> PositiveNormal

  equalByTotalOrder x y = castDoubleToWord64 x == castDoubleToWord64 y

  compareByTotalOrder x y = let x' = castDoubleToWord64 x
                                y' = castDoubleToWord64 y
                            in compare (testBit y' 63) (testBit x' 63) -- sign bit
                               <> if testBit x' 63 then
                                    compare y' x' -- negative
                                  else
                                    compare x' y' -- positive

-- | A newtype wrapper to compare floating-point numbers by @totalOrder@ predicate.
newtype TotallyOrdered a = TotallyOrdered a
  deriving (Show)

instance RealFloatNaN a => Eq (TotallyOrdered a) where
  TotallyOrdered x == TotallyOrdered y = equalByTotalOrder x y

instance RealFloatNaN a => Ord (TotallyOrdered a) where
  compare (TotallyOrdered x) (TotallyOrdered y) = compareByTotalOrder x y
