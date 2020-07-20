{-# LANGUAGE NumericUnderscores #-}
module Numeric.Floating.IEEE.NaN
  ( SupportsNaN(..)
  , classify
  , Class (..)
  , TotallyOrdered(..)
  , compareByTotalOrder
  ) where
import           Data.Bits
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                                   castWord32ToFloat, castWord64ToDouble)
import           Numeric.Floating.IEEE.Internal.Classify (Class (..))

-- | An instance of this class supports manipulation of NaN.
class SupportsNaN a where
  -- 5.5.1 Sign bit operations
  -- | Returns the first operand, with the sign of the second
  --
  -- IEEE 754 @copySign@ operation.
  copySign :: a -> a -> a

  -- 5.7.2 General operations
  -- |
  -- IEEE 754 @isSignMinus@ operation.
  isSignMinus :: a -> Bool

  -- |
  -- IEEE 754 @isSignaling@ operation.
  isSignaling :: a -> Bool

  -- 9.7 NaN payload operations

  -- |
  -- IEEE 754 @getPayload@ operation.
  getPayload :: a -> a

  -- |
  -- IEEE 754 @setPayload@ operation.
  setPayload :: a -> a

  -- |
  -- IEEE 754 @setPayloadSignaling@ operation.
  setPayloadSignaling :: a -> a

instance SupportsNaN Float where
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

instance SupportsNaN Double where
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

classify :: (RealFloat a, SupportsNaN a) => a -> Class
classify x | isNaN x                 = if isSignaling x then
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
                        (_,     0xff, _) -> if testBit w 22 then
                                              QuietNaN
                                            else
                                              SignalingNaN
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
                        (_,     0x7ff, _) -> if testBit w 51 then
                                               QuietNaN
                                             else
                                               SignalingNaN
                        (True,  _,     _) -> NegativeNormal
                        (False, _,     _) -> PositiveNormal

-- A newtype wrapper to compare by totalOrder predicate
newtype TotallyOrdered a = TotallyOrdered a
  deriving (Show)

instance (RealFloat a, SupportsNaN a) => Eq (TotallyOrdered a) where
  TotallyOrdered x == TotallyOrdered y = compareByTotalOrder x y == EQ -- TODO: More efficient implementation

instance (RealFloat a, SupportsNaN a) => Ord (TotallyOrdered a) where
  compare (TotallyOrdered x) (TotallyOrdered y) = compareByTotalOrder x y

compareByTotalOrder :: (RealFloat a, SupportsNaN a) => a -> a -> Ordering
compareByTotalOrder x y
  | x < y = LT
  | y < x = GT
  | x == y = if x == 0 then
               compare (isNegativeZero y) (isNegativeZero x)
             else
               EQ -- TODO: non-canonical?
  | otherwise = compare (isSignMinus y) (isSignMinus x)
                <> let r = compare (isNaN x) (isNaN y) -- number < +NaN
                           <> compare (isSignaling y) (isSignaling x) -- +(signaling NaN) < +(quiet NaN)
                           <> compare (getPayload x) (getPayload y) -- implementation-defined
                   in if isSignMinus x then
                        compare EQ r
                      else
                        r
{-# NOINLINE [1] compareByTotalOrder #-}
{-# RULES
"compareByTotalOrder/Float" compareByTotalOrder = compareByTotalOrderFloat
"compareByTotalOrder/Double" compareByTotalOrder = compareByTotalOrderDouble
  #-}

compareByTotalOrderFloat :: Float -> Float -> Ordering
compareByTotalOrderFloat x y = let x' = castFloatToWord32 x
                                   y' = castFloatToWord32 y
                               in compare (x' .&. 0x8000_0000) (y' .&. 0x8000_0000) -- sign bit
                                  <> if testBit x' 31 then
                                       compare y' x' -- negative
                                     else
                                       compare x' y' -- positive

compareByTotalOrderDouble :: Double -> Double -> Ordering
compareByTotalOrderDouble x y = let x' = castDoubleToWord64 x
                                    y' = castDoubleToWord64 y
                                in compare (x' .&. 0x8000_0000_0000_000) (y' .&. 0x8000_0000_0000_000) -- sign bit
                                   <> if testBit x' 63 then
                                        compare y' x' -- negative
                                      else
                                        compare x' y' -- positive
