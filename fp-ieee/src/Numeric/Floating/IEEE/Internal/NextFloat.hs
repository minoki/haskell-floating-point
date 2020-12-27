{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
module Numeric.Floating.IEEE.Internal.NextFloat where
import           Data.Bits
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                                   castWord32ToFloat, castWord64ToDouble)
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base

default ()

-- $setup
-- >>> :set -XHexFloatLiterals -XNumericUnderscores

-- |
-- Returns the smallest value that is larger than the argument.
--
-- IEEE 754 @nextUp@ operation.
--
-- >>> nextUp 1 == (0x1.000002p0 :: Float)
-- True
-- >>> nextUp 1 == (0x1.0000_0000_0000_1p0 :: Double)
-- True
-- >>> nextUp (1/0) == (1/0 :: Double)
-- True
-- >>> nextUp (-1/0) == (- maxFinite :: Double)
-- True
-- >>> nextUp 0 == (0x1p-1074 :: Double)
-- True
-- >>> nextUp (-0) == (0x1p-1074 :: Double)
-- True
-- >>> nextUp (-0x1p-1074) :: Double -- returns negative zero
-- -0.0
nextUp :: RealFloat a => a -> a
nextUp x | not (isIEEE x) = error "non-IEEE numbers are not supported"
         | isNaN x || (isInfinite x && x > 0) = x + x -- NaN or positive infinity
         | x >= 0 = nextUp_positive x
         | otherwise = - nextDown_positive (- x)
{-# INLINE [1] nextUp #-}

-- |
-- Returns the largest value that is smaller than the argument.
--
-- IEEE 754 @nextDown@ operation.
--
-- >>> nextDown 1 == (0x1.ffff_ffff_ffff_fp-1 :: Double)
-- True
-- >>> nextDown 1 == (0x1.fffffep-1 :: Float)
-- True
-- >>> nextDown (1/0) == (maxFinite :: Double)
-- True
-- >>> nextDown (-1/0) == (-1/0 :: Double)
-- True
-- >>> nextDown 0 == (-0x1p-1074 :: Double)
-- True
-- >>> nextDown (-0) == (-0x1p-1074 :: Double)
-- True
-- >>> nextDown 0x1p-1074 -- returns positive zero
-- 0.0
-- >>> nextDown 0x1p-1022 == (0x0.ffff_ffff_ffff_fp-1022 :: Double)
-- True
nextDown :: RealFloat a => a -> a
nextDown x | not (isIEEE x) = error "non-IEEE numbers are not supported"
           | isNaN x || (isInfinite x && x < 0) = x + x -- NaN or negative infinity
           | x >= 0 = nextDown_positive x
           | otherwise = - nextUp_positive (- x)
{-# INLINE [1] nextDown #-}

-- |
-- Returns the value whose magnitude is smaller than that of the argument, and is closest to the argument.
--
-- This operation is not in IEEE, but may be useful to some.
--
-- >>> nextTowardZero 1 == (0x1.ffff_ffff_ffff_fp-1 :: Double)
-- True
-- >>> nextTowardZero 1 == (0x1.fffffep-1 :: Float)
-- True
-- >>> nextTowardZero (1/0) == (maxFinite :: Double)
-- True
-- >>> nextTowardZero (-1/0) == (-maxFinite :: Double)
-- True
-- >>> nextTowardZero 0 :: Double -- returns positive zero
-- 0.0
-- >>> nextTowardZero (-0 :: Double) -- returns negative zero
-- -0.0
-- >>> nextTowardZero 0x1p-1074 :: Double
-- 0.0
nextTowardZero :: RealFloat a => a -> a
nextTowardZero x | not (isIEEE x) = error "non-IEEE numbers are not supported"
                 | isNaN x || x == 0 = x + x -- NaN or zero
                 | x >= 0 = nextDown_positive x
                 | otherwise = - nextDown_positive (- x)
{-# INLINE [1] nextTowardZero #-}

nextUp_positive :: RealFloat a => a -> a
nextUp_positive x
  | isNaN x || x < 0 = error "nextUp_positive"
  | isInfinite x = x
  | x == 0 = encodeFloat 1 (expMin - d) -- min positive
  | otherwise = let m :: Integer
                    e :: Int
                    (m,e) = decodeFloat x
                    -- x = m * 2^e, 2^(d-1) <= m < 2^d
                    -- 2^expMin < x < 2^expMax
                    -- 2^(expMin-d): min positive
                    -- 2^(expMin - 1): min normal 0x1p-1022
                    -- expMin - d <= e <= expMax - d (-1074 .. 971)
                in if expMin - d <= e then
                     -- normal
                     if m + 1 == base ^! d && e == expMax - d then
                       1 / 0 -- max finite -> infinity
                     else
                       encodeFloat (m + 1) e
                   else
                     -- subnormal
                     let m' = if base == 2 then
                                m `unsafeShiftR` (expMin - d - e)
                              else
                                m `quot` (base ^ (expMin - d - e))
                     in encodeFloat (m' + 1) (expMin - d)
  where
    d, expMin :: Int
    base = floatRadix x
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
{-# INLINE nextUp_positive #-}

nextDown_positive :: RealFloat a => a -> a
nextDown_positive x
  | isNaN x || x < 0 = error "nextDown_positive"
  | isInfinite x = maxFinite
  | x == 0 = encodeFloat (-1) (expMin - d) -- max negative
  | otherwise = let m :: Integer
                    e :: Int
                    (m,e) = decodeFloat x
                    -- x = m * 2^e, 2^(d-1) <= m < 2^d
                    -- 2^expMin < x < 2^expMax
                    -- 2^(expMin-d): min positive
                    -- 2^(expMin - 1): min normal 0x1p-1022
                    -- expMin - d <= e <= expMax - d (-1074 .. 971)
                in if expMin - d <= e then
                     -- normal
                     let m1 = m - 1
                     in if m == base ^! (d - 1) && expMin - d /= e then
                          encodeFloat (base * m - 1) (e - 1)
                        else
                          encodeFloat m1 e
                   else
                     -- subnormal
                     let m' = if base == 2 then
                                m `unsafeShiftR` (expMin - d - e)
                              else
                                m `quot` (base ^ (expMin - d - e))
                     in encodeFloat (m' - 1) (expMin - d)
  where
    d, expMin :: Int
    base = floatRadix x
    d = floatDigits x -- 53 for Double
    (expMin,_expMax) = floatRange x -- (-1021,1024) for Double
{-# INLINE nextDown_positive #-}

{-# RULES
"nextUp/Float" nextUp = nextUpFloat
"nextUp/Double" nextUp = nextUpDouble
"nextDown/Float" nextDown = nextDownFloat
"nextDown/Double" nextDown = nextDownDouble
"nextTowardZero/Float" nextTowardZero = nextTowardZeroFloat
"nextTowardZero/Double" nextTowardZero = nextTowardZeroDouble
  #-}

-- |
-- prop> nextUpFloat 1 == 0x1.000002p0
-- prop> nextUpFloat (1/0) == 1/0
-- prop> nextUpFloat (-1/0) == - maxFinite
-- prop> nextUpFloat 0 == 0x1p-149
-- prop> nextUpFloat (-0) == 0x1p-149
-- prop> isNegativeZero (nextUpFloat (-0x1p-149))
nextUpFloat :: Float -> Float
nextUpFloat x =
  case castFloatToWord32 x of
    w | w .&. 0x7f80_0000 == 0x7f80_0000
      , w /= 0xff80_0000 -> x + x -- NaN or positive infinity -> itself
    0x8000_0000 -> minPositive -- -0 -> min positive
    w | testBit w 31 -> castWord32ToFloat (w - 1) -- negative
      | otherwise -> castWord32ToFloat (w + 1) -- positive
  where
    !True = isFloatBinary32 || error "Numeric.Floating.Extra assumes Float is IEEE binary32"

-- |
-- prop> nextUpDouble 1 == 0x1.0000_0000_0000_1p0
-- prop> nextUpDouble (1/0) == 1/0
-- prop> nextUpDouble (-1/0) == - maxFinite
-- prop> nextUpDouble 0 == 0x1p-1074
-- prop> nextUpDouble (-0) == 0x1p-1074
-- prop> isNegativeZero (nextUpDouble (-0x1p-1074))
nextUpDouble :: Double -> Double
nextUpDouble x =
  case castDoubleToWord64 x of
    w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
      , w /= 0xfff0_0000_0000_0000 -> x + x -- NaN or positive infinity -> itself
    0x8000_0000_0000_0000 -> minPositive -- -0 -> min positive
    w | testBit w 63 -> castWord64ToDouble (w - 1) -- negative
      | otherwise -> castWord64ToDouble (w + 1) -- positive
  where
     !True = isDoubleBinary64 || error "Numeric.Floating.Extra assumes Double is IEEE binary64"

-- |
-- prop> nextDownFloat 1 == 0x1.fffffep-1
-- prop> nextDownFloat (1/0) == maxFinite
-- prop> nextDownFloat (-1/0) == -1/0
-- prop> nextDownFloat 0 == -0x1p-149
-- prop> nextDownFloat (-0) == -0x1p-149
-- prop> nextDownFloat 0x1p-149 == 0
nextDownFloat :: Float -> Float
nextDownFloat x =
  case castFloatToWord32 x of
    w | w .&. 0x7f80_0000 == 0x7f80_0000
      , w /= 0x7f80_0000 -> x + x -- NaN or negative infinity -> itself
    0x0000_0000 -> - minPositive -- +0 -> max negative
    w | testBit w 31 -> castWord32ToFloat (w + 1) -- negative
      | otherwise -> castWord32ToFloat (w - 1) -- positive
  where
    !True = isFloatBinary32 || error "Numeric.Floating.Extra assumes Float is IEEE binary32"

-- |
-- prop> nextDownDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextDownDouble (1/0) == maxFinite
-- prop> nextDownDouble (-1/0) == -1/0
-- prop> nextDownDouble 0 == -0x1p-1074
-- prop> nextDownDouble (-0) == -0x1p-1074
-- prop> nextDownDouble 0x1p-1074 == 0
nextDownDouble :: Double -> Double
nextDownDouble x =
  case castDoubleToWord64 x of
    w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
      , w /= 0x7ff0_0000_0000_0000 -> x + x -- NaN or negative infinity -> itself
    0x0000_0000_0000_0000 -> - minPositive -- +0 -> max negative
    w | testBit w 63 -> castWord64ToDouble (w + 1) -- negative
      | otherwise -> castWord64ToDouble (w - 1) -- positive
  where
     !True = isDoubleBinary64 || error "Numeric.Floating.Extra assumes Double is IEEE binary64"

-- |
-- prop> nextTowardZeroFloat 1 == 0x1.fffffep-1
-- prop> nextTowardZeroFloat (-1) == -0x1.fffffep-1
-- prop> nextTowardZeroFloat (1/0) == maxFinite
-- prop> nextTowardZeroFloat (-1/0) == -maxFinite
-- prop> nextTowardZeroFloat 0 == 0
-- prop> isNegativeZero (nextTowardZeroFloat (-0))
-- prop> nextTowardZeroFloat 0x1p-149 == 0
nextTowardZeroFloat :: Float -> Float
nextTowardZeroFloat x =
  case castFloatToWord32 x of
    w | w .&. 0x7f80_0000 == 0x7f80_0000
      , w .&. 0x007f_ffff /= 0 -> x + x -- NaN -> itself
    0x8000_0000 -> x -- -0 -> itself
    0x0000_0000 -> x -- +0 -> itself
    w -> castWord32ToFloat (w - 1) -- positive / negative
  where
    !True = isFloatBinary32 || error "Numeric.Floating.Extra assumes Float is IEEE binary32"

-- |
-- prop> nextTowardZeroDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (-1) == -0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (1/0) == maxFinite
-- prop> nextTowardZeroDouble (-1/0) == -maxFinite
-- prop> nextTowardZeroDouble 0 == 0
-- prop> isNegativeZero (nextTowardZeroDouble (-0))
-- prop> nextTowardZeroDouble 0x1p-1074 == 0
nextTowardZeroDouble :: Double -> Double
nextTowardZeroDouble x =
  case castDoubleToWord64 x of
    w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
      , w .&. 0x000f_ffff_ffff_ffff /= 0 -> x + x -- NaN -> itself
    0x8000_0000_0000_0000 -> x -- -0 -> itself
    0x0000_0000_0000_0000 -> x -- +0 -> itself
    w -> castWord64ToDouble (w - 1) -- positive / negative
  where
    !True = isDoubleBinary64 || error "Numeric.Floating.Extra assumes Double is IEEE binary64"
