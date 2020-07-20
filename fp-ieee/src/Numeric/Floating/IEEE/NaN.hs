{-# LANGUAGE NumericUnderscores #-}
module Numeric.Floating.IEEE.NaN where
import           Data.Bits
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                                   castWord32ToFloat, castWord64ToDouble)
import           Numeric.Floating.IEEE (Class (..))

class SupportsNaN a where
  -- 5.5.1 Sign bit operations
  -- | Returns the first operand, with the sign of the second
  copySign :: a -> a -> a

  -- 5.7.2 General operations
  isSignMinus :: a -> Bool
  isSignaling :: a -> Bool

  -- 9.7 NaN payload operations
  getPayload :: a -> a
  setPayload :: a -> a
  setPayloadSignaling :: a -> a

-- not implemented yet:
-- instance SupportsNaN Float where

instance SupportsNaN Double where
  copySign x y = castWord64ToDouble ((x' .&. 0x7fff_ffff_ffff_ffff) .|. (y' .&. 0x8000_0000_0000_0000))
    where x' = castDoubleToWord64 x
          y' = castDoubleToWord64 y
  isSignMinus x = testBit (castDoubleToWord64 x) 63
  isSignaling x = x' .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000 && x' /= 0x7ff0_0000_0000_0000 && not (testBit x' 51)
    where x' = castDoubleToWord64 x
  getPayload x
    | not (isNaN x) = -1
    | otherwise = fromIntegral (castDoubleToWord64 x .&. 0x0007_ffff_ffff_ffff)
  setPayload x
    | x >= 0 = castWord64ToDouble $ (round x .&. 0x0007_ffff_ffff_ffff) .|. 0x7ff8_0000_0000_0000
    | otherwise = 0
  setPayloadSignaling x
    | x >= 0 = castWord64ToDouble $ (round x .&. 0x0007_ffff_ffff_ffff) .|. 0x7ff0_0000_0000_0000
    | otherwise = 0

classify :: (RealFloat a, SupportsNaN a) => a -> Class
classify x | isNaN x = if isSignaling x then
                         SignalingNaN
                       else
                         QuietNaN
           | x < 0, isInfinite x = NegativeInfinity
           | x < 0, isDenormalized x = NegativeSubnormal
           | x < 0 = NegativeNormal
           | isNegativeZero x = NegativeZero
           | x == 0 = PositiveZero
           | isDenormalized x = PositiveSubnormal
           | isInfinite x = PositiveInfinity
           | otherwise = PositiveNormal
-- TODO: Specialized version for Float, Double

-- isSignMinus, isSignaling: class method

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
               EQ -- TODO
  | otherwise = compare (isSignMinus y) (isSignMinus x)
                <> let r = compare (isNaN x) (isNaN y) -- number < +NaN
                           <> compare (isSignaling y) (isSignaling x) -- +(signaling NaN) < +(quiet NaN)
                           <> compare (getPayload x) (getPayload y) -- implementation-defined
                   in if isSignMinus x then
                        compare EQ r
                      else
                        r

compareByTotalOrderDouble :: Double -> Double -> Ordering
compareByTotalOrderDouble x y = let x' = castDoubleToWord64 x
                                    y' = castDoubleToWord64 y
                                in compare (x' .&. 0x8000_0000_0000_000) (y' .&. 0x8000_0000_0000_000) -- sign bit
                                   <> if testBit x' 63 then
                                        compare y' x' -- negative
                                      else
                                        compare x' y' -- positive
