{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
module Numeric.Floating.IEEE.Internal.Half where
import           Data.Bits
import           Data.Coerce
import           Data.Word
import           Foreign.C.Types
import           GHC.Exts
import           GHC.Float.Compat (float2Double)
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.Classify
import           Numeric.Floating.IEEE.Internal.Conversion
import           Numeric.Floating.IEEE.Internal.FMA
import           Numeric.Floating.IEEE.Internal.NaN (RealFloatNaN)
import qualified Numeric.Floating.IEEE.Internal.NaN as NaN
import           Numeric.Floating.IEEE.Internal.NextFloat
import           Numeric.Floating.IEEE.Internal.Rounding
import           Numeric.Half hiding (isZero)
import qualified Numeric.Half

default ()

castHalfToWord16 :: Half -> Word16
castHalfToWord16 (Half x) = coerce x
{-# INLINE castHalfToWord16 #-}

castWord16ToHalf :: Word16 -> Half
castWord16ToHalf x = Half (coerce x)
{-# INLINE castWord16ToHalf #-}

nextUpHalf :: Half -> Half
nextUpHalf x =
  case castHalfToWord16 x of
    w | w .&. 0x7c00 == 0x7c00
      , w /= 0xfc00 -> x -- NaN or negative infinity -> itself
    0x8000 -> minPositive -- -0 -> min positive
    w | testBit w 15 -> castWord16ToHalf (w - 1) -- negative
      | otherwise -> castWord16ToHalf (w + 1) -- positive

nextDownHalf :: Half -> Half
nextDownHalf x =
  case castHalfToWord16 x of
    w | w .&. 0x7c00 == 0x7c00
      , w /= 0x7c00 -> x -- NaN or positive infinity -> itself
    0x0000 -> - minPositive -- +0 -> max negative
    w | testBit w 15 -> castWord16ToHalf (w + 1) -- negative
      | otherwise -> castWord16ToHalf (w - 1) -- positive

nextTowardZeroHalf :: Half -> Half
nextTowardZeroHalf x =
  case castHalfToWord16 x of
    w | w .&. 0x7c00 == 0x7c00
      , w /= 0x7fff -> x -- NaN -> itself
    0x8000 -> x -- -0 -> itself
    0x0000 -> x -- +0 -> itself
    w -> castWord16ToHalf (w - 1) -- positive / negative

isNormalHalf :: Half -> Bool
isNormalHalf x = let w = castHalfToWord16 x .&. 0x7c00
                 in w /= 0 && w /= 0x7c00

isFiniteHalf :: Half -> Bool
isFiniteHalf x = let w = castHalfToWord16 x .&. 0x7c00
                 in w /= 0x7c00

isSignMinusHalf :: Half -> Bool
isSignMinusHalf x = let w = castHalfToWord16 x
                    in testBit w 15 && (w .&. 0x7c00 /= 0x7c00 || w .&. 0x3ff == 0) -- all NaNs are treated as positive

classifyHalf :: Half -> Class
classifyHalf x = let w = castHalfToWord16 x
                     s = testBit w 15
                     e = (w `unsafeShiftR` 10) .&. 0x1f -- exponent (5 bits)
                     m = w .&. 0x3ff -- mantissa (10 bits without leading 1)
                 in case (s, e, m) of
                      (True,  0,    0) -> NegativeZero
                      (False, 0,    0) -> PositiveZero
                      (True,  0,    _) -> NegativeSubnormal
                      (False, 0,    _) -> PositiveSubnormal
                      (True,  0x1f, 0) -> NegativeInfinity
                      (False, 0x1f, 0) -> PositiveInfinity
                      (_,     0x1f, _) -> QuietNaN -- treat all NaNs as quiet
                      (True,  _,    _) -> NegativeNormal
                      (False, _,    _) -> PositiveNormal

instance RealFloatNaN Half where
  copySign x y = castWord16ToHalf ((x' .&. 0x7fff) .|. (y' .&. 0x8000))
    where x' = castHalfToWord16 x
          y' = castHalfToWord16 y

  isSignMinus x = testBit (castHalfToWord16 x) 15

  isSignaling x = x' .&. 0x7c00 == 0x7c00 && x' .&. 0x7fff /= 0x7c00 && not (testBit x' 9)
    where x' = castHalfToWord16 x

  getPayload x
    | not (isNaN x) = -1
    | otherwise = fromIntegral (castHalfToWord16 x .&. 0x01ff)

  setPayload x
    | 0 <= x && x <= 0x01ff = castWord16ToHalf $ round x .|. 0x7e00
    | otherwise = 0

  setPayloadSignaling x
    | 0 < x && x <= 0x01ff = castWord16ToHalf $ round x .|. 0x7c00
    | otherwise = 0

  classify x =
    let w = castHalfToWord16 x
        s = testBit w 15
        e = (w `unsafeShiftR` 10) .&. 0x1f -- exponent (5 bits)
        m = w .&. 0x3ff -- mantissa (10 bits without leading 1)
    in case (s, e, m) of
         (True,  0,    0) -> NegativeZero
         (False, 0,    0) -> PositiveZero
         (True,  0,    _) -> NegativeSubnormal
         (False, 0,    _) -> PositiveSubnormal
         (True,  0x1f, 0) -> NegativeInfinity
         (False, 0x1f, 0) -> PositiveInfinity
         (_,     0x1f, _) -> if testBit w 9 then
                               QuietNaN
                             else
                               SignalingNaN
         (True,  _,    _) -> NegativeNormal
         (False, _,    _) -> PositiveNormal

  compareByTotalOrder x y =
    let x' = castHalfToWord16 x
        y' = castHalfToWord16 y
    in compare (x' .&. 0x8000) (y' .&. 0x8000) -- sign bit
       <> if testBit x' 15 then
            compare y' x' -- negative
          else
            compare x' y' -- positive

{-# RULES
"nextUp/Half" nextUp = nextUpHalf
"nextDown/Half" nextDown = nextDownHalf
"nextTowardZero/Half" nextTowardZero = nextTowardZeroHalf
"isNormal/Half" isNormal = isNormalHalf
"isFinite/Half" isFinite = isFiniteHalf
"isZero/Half" isZero = Numeric.Half.isZero
"isSignMinus/Half" isSignMinus = isSignMinusHalf
"classify/Half" classify = classifyHalf
"isMantissaEven/Half" forall (x :: Half).
  isMantissaEven x = even (castHalfToWord16 x)
  #-}

{-# SPECIALIZE minPositive :: Half #-}
{-# SPECIALIZE minPositiveNormal :: Half #-}
{-# SPECIALIZE maxFinite :: Half #-}
{-# SPECIALIZE
  positiveWordToBinaryFloatR# :: RoundingStrategy f => Bool -> Word# -> f Half
                               , Bool -> Word# -> RoundTiesToEven Half
                               , Bool -> Word# -> RoundTiesToAway Half
                               , Bool -> Word# -> RoundTowardPositive Half
                               , Bool -> Word# -> RoundTowardNegative Half
                               , Bool -> Word# -> RoundTowardZero Half
  #-}
{-# SPECIALIZE
  fromPositiveIntegerR :: RoundingStrategy f => Bool -> Integer -> f Half
                        , Bool -> Integer -> RoundTiesToEven Half
                        , Bool -> Integer -> RoundTiesToAway Half
                        , Bool -> Integer -> RoundTowardPositive Half
                        , Bool -> Integer -> RoundTowardNegative Half
                        , Bool -> Integer -> RoundTowardZero Half
  #-}
{-# SPECIALIZE
  fromPositiveRatioR :: RoundingStrategy f => Bool -> Integer -> Integer -> f Half
                      , Bool -> Integer -> Integer -> RoundTiesToEven Half
                      , Bool -> Integer -> Integer -> RoundTiesToAway Half
                      , Bool -> Integer -> Integer -> RoundTowardPositive Half
                      , Bool -> Integer -> Integer -> RoundTowardNegative Half
                      , Bool -> Integer -> Integer -> RoundTowardZero Half
  #-}
{-# SPECIALIZE
  encodePositiveFloatR# :: RoundingStrategy f => Bool -> Integer -> Int# -> f Half
                         , Bool -> Integer -> Int# -> RoundTiesToEven Half
                         , Bool -> Integer -> Int# -> RoundTiesToAway Half
                         , Bool -> Integer -> Int# -> RoundTowardPositive Half
                         , Bool -> Integer -> Int# -> RoundTowardNegative Half
                         , Bool -> Integer -> Int# -> RoundTowardZero Half
  #-}
{-# SPECIALIZE
  scaleFloatR# :: RoundingStrategy f => Int# -> Half -> f Half
                , Int# -> Half -> RoundTiesToEven Half
                , Int# -> Half -> RoundTiesToAway Half
                , Int# -> Half -> RoundTowardPositive Half
                , Int# -> Half -> RoundTowardNegative Half
                , Int# -> Half -> RoundTowardZero Half
  #-}

-- Monomorphic conversion functions
halfToFloat :: Half -> Float
halfToDouble :: Half -> Double
floatToHalf :: Float -> Half
doubleToHalf :: Double -> Half

#if defined(HAS_FAST_HALF_CONVERSION)

foreign import ccall unsafe "hs_fastHalfToFloat"
  c_fastHalfToFloat :: Word16 -> Float
foreign import ccall unsafe "hs_fastHalfToDouble"
  c_fastHalfToDouble :: Word16 -> Double
foreign import ccall unsafe "hs_fastFloatToHalf"
  c_fastFloatToHalf :: Float -> Word16
foreign import ccall unsafe "hs_fastDoubleToHalf"
  c_fastDoubleToHalf :: Double -> Word16

halfToFloat = coerce c_fastHalfToFloat
{-# INLINE halfToFloat #-}

halfToDouble = coerce c_fastHalfToDouble
{-# INLINE halfToDouble #-}

floatToHalf = coerce c_fastFloatToHalf
{-# INLINE floatToHalf #-}

doubleToHalf = coerce c_fastDoubleToHalf
{-# INLINE doubleToHalf #-}

{-# RULES
"realFloatToFrac/Half->Float" realFloatToFrac = halfToFloat
"realFloatToFrac/Half->Double" realFloatToFrac = halfToDouble
"realFloatToFrac/Float->Half" realFloatToFrac = floatToHalf
"realFloatToFrac/Double->Half" realFloatToFrac = doubleToHalf
  #-}

#else

halfToFloat = fromHalf
{-# INLINE halfToFloat #-}

halfToDouble = float2Double . fromHalf
{-# INLINE halfToDouble #-}

floatToHalf = toHalf
{-# INLINE floatToHalf #-}

doubleToHalf = realFloatToFrac -- generic implementation
{-# INLINE doubleToHalf #-}

{-# RULES
"realFloatToFrac/Half->Float" realFloatToFrac = fromHalf
"realFloatToFrac/Half->Double" realFloatToFrac = (realFloatToFrac . fromHalf) :: Half -> Double
"realFloatToFrac/Float->Half" realFloatToFrac = toHalf
  #-}

#endif
