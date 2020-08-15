{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
module Numeric.Floating.IEEE.Internal.Float128 where
import           Data.Bits
import           Data.Word
import           GHC.Exts (Int#)
import           MyPrelude
import           Numeric.Float128 (Float128 (F128))
import qualified Numeric.Float128
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.Classify
import           Numeric.Floating.IEEE.Internal.Conversion
import           Numeric.Floating.IEEE.Internal.FMA
import           Numeric.Floating.IEEE.Internal.NaN (RealFloatNaN)
import qualified Numeric.Floating.IEEE.Internal.NaN as NaN
import           Numeric.Floating.IEEE.Internal.NextFloat
import           Numeric.Floating.IEEE.Internal.Rounding
import           Numeric.Floating.IEEE.Internal.RoundToIntegral

default ()

{-
Float128:
- exponent = 15 bits
- precision = 113 bits
-}

float128ToWord64Hi, float128ToWord64Lo :: Float128 -> Word64
float128ToWord64Hi (F128 hi _lo) = hi
float128ToWord64Lo (F128 _hi lo) = lo
{-# INLINE float128ToWord64Hi #-}
{-# INLINE float128ToWord64Lo #-}

float128ToWord64Pair :: Float128 -> (Word64, Word64)
float128ToWord64Pair (F128 hi lo) = (hi, lo)
{-# INLINE float128ToWord64Pair #-}

float128FromWord64Pair :: Word64 -- ^ higher 64 bits
                       -> Word64 -- ^ lower 64 bits
                       -> Float128
float128FromWord64Pair hi lo = F128 hi lo
{-# INLINE float128FromWord64Pair #-}

succWord64Pair :: Word64 -> Word64 -> (Word64, Word64)
succWord64Pair hi lo | lo + 1 == 0 = (hi + 1, 0)
                     | otherwise = (hi, lo + 1)

predWord64Pair :: Word64 -> Word64 -> (Word64, Word64)
predWord64Pair hi lo | lo == 0 = (hi - 1, fromInteger (-1))
                     | otherwise = (hi, lo - 1)

nextUpF128 :: Float128 -> Float128
nextUpF128 x =
  case float128ToWord64Pair x of
    (hi, lo) | hi .&. 0x7fff_0000_0000_0000 == 0x7fff_0000_0000_000
             , (hi, lo) /= (0xffff_0000_0000_0000, 0) -> x + x -- NaN or positive infinity -> itself
    (0x8000_0000_0000_0000, 0x0000_0000_0000_0000) -> minPositive -- -0 -> min positive
    (hi, lo) | testBit hi 63 -> -- negative
                 case predWord64Pair hi lo of
                   (hi', lo') -> float128FromWord64Pair hi' lo'
             | otherwise -> -- positive
                 case succWord64Pair hi lo of
                   (hi', lo') -> float128FromWord64Pair hi' lo'

nextDownF128 :: Float128 -> Float128
nextDownF128 x =
  case float128ToWord64Pair x of
    (hi, lo) | hi .&. 0x7fff_0000_0000_0000 == 0x7fff_0000_0000_000
             , (hi, lo) /= (0x7fff_0000_0000_0000, 0) -> x + x -- NaN or negative infinity -> itself
    (0x0000_0000_0000_0000, 0x0000_0000_0000_0000) -> - minPositive -- +0 -> max negative
    (hi, lo) | testBit hi 63 -> -- negative
                 case succWord64Pair hi lo of
                   (hi', lo') -> float128FromWord64Pair hi' lo'
             | otherwise -> -- positive
                 case predWord64Pair hi lo of
                   (hi', lo') -> float128FromWord64Pair hi' lo'

nextTowardZeroF128 :: Float128 -> Float128
nextTowardZeroF128 x =
  case float128ToWord64Pair x of
    (hi, lo) | hi .&. 0x7fff_0000_0000_0000 == 0x7fff_0000_0000_000
             , (lo, hi .&. 0x0000_ffff_ffff_ffff) /= (0, 0) -> x + x -- NaN -> itself
    (0x8000_0000_0000_0000, 0x0000_0000_0000_0000) -> x -- -0 -> itself
    (0x0000_0000_0000_0000, 0x0000_0000_0000_0000) -> x -- +0 -> itself
    (hi, lo) -> -- positive / negative
      case predWord64Pair hi lo of
        (hi', lo') -> float128FromWord64Pair hi' lo'

isNormalF128 :: Float128 -> Bool
isNormalF128 x = case float128ToWord64Pair x of
                   (hi, _) -> let hi' = hi .&. 0x7fff_0000_0000_0000
                              in hi' /= 0 && hi' /= 0x7fff_0000_0000_0000

isFiniteF128 :: Float128 -> Bool
isFiniteF128 x = case float128ToWord64Pair x of
                   (hi, _) -> let hi' = hi .&. 0x7fff_0000_0000_0000
                              in hi' /= 0 && hi' /= 0x7fff_0000_0000_0000

classifyF128DiscardingSignalingNaNs :: Float128 -> Class
classifyF128DiscardingSignalingNaNs x =
  let hi = float128ToWord64Hi x
      s = testBit hi 63
      e = (hi `unsafeShiftR` 48) .&. 0x7fff -- exponent (15 bits)
      m_hi = hi .&. 0x0000_ffff_ffff_ffff
      m_lo = float128ToWord64Lo x
  in case (s, e, m_hi, m_lo) of
       (True,  0,      0, 0) -> NegativeZero
       (False, 0,      0, 0) -> PositiveZero
       (True,  0,      _, _) -> NegativeSubnormal
       (False, 0,      _, _) -> PositiveSubnormal
       (True,  0x7fff, 0, 0) -> NegativeInfinity
       (False, 0x7fff, 0, 0) -> PositiveInfinity
       (_,     0x7fff, _, _) -> QuietNaN -- treat all NaNs as quiet
       (True,  _,      _, _) -> NegativeNormal
       (False, _,      _, _) -> PositiveNormal

instance RealFloatNaN Float128 where
  copySign x y = let (x_hi, x_lo) = float128ToWord64Pair x
                     y_hi = float128ToWord64Hi y
                 in float128FromWord64Pair ((x_hi .&. 0x7fff_ffff_ffff_ffff) .|. (y_hi .&. 0x8000_0000_0000_0000)) x_lo
  isSignMinus x = let hi = float128ToWord64Hi x
                  in testBit hi 63
  isSignaling x = let hi = float128ToWord64Hi x
                  in isNaN x && not (testBit hi 47)

  getPayload x
    | not (isNaN x) = -1
    | otherwise = let hi = fromIntegral (float128ToWord64Hi x .&. 0x0000_7fff_ffff_ffff)
                      lo = fromIntegral (float128ToWord64Lo x)
                  in hi * 0x1_0000_0000_0000_0000 + lo

  setPayload x
    | 0 <= x && x <= 0x0000_7fff_ffff_ffff_ffff_ffff_ffff_ffff
    = let payloadI = round x
          hi = fromInteger (payloadI `shiftR` 64) .|. 0x7fff_8000_0000_0000
          lo = fromInteger (payloadI .&. 0xffff_ffff_ffff_ffff)
      in float128FromWord64Pair hi lo
    | otherwise = 0

  setPayloadSignaling x
    | 0 < x && x <= 0x0000_7fff_ffff_ffff_ffff_ffff_ffff_ffff
    = let payloadI = round x
          hi = fromInteger (payloadI `shiftR` 64) .|. 0x7fff_0000_0000_0000
          lo = fromInteger (payloadI .&. 0xffff_ffff_ffff_ffff)
      in float128FromWord64Pair hi lo
    | otherwise = 0

  classify x =
    let hi = float128ToWord64Hi x
        s = testBit hi 63
        e = (hi `unsafeShiftR` 48) .&. 0x7fff -- exponent (15 bits)
        m_hi = hi .&. 0x0000_ffff_ffff_ffff
        m_lo = float128ToWord64Lo x
    in case (s, e, m_hi, m_lo) of
         (True,  0,      0, 0) -> NegativeZero
         (False, 0,      0, 0) -> PositiveZero
         (True,  0,      _, _) -> NegativeSubnormal
         (False, 0,      _, _) -> PositiveSubnormal
         (True,  0x7fff, 0, 0) -> NegativeInfinity
         (False, 0x7fff, 0, 0) -> PositiveInfinity
         (_,     0x7fff, _, _) -> if testBit m_hi 47 then
                                    QuietNaN
                                  else
                                    SignalingNaN
         (True,  _,      _, _) -> NegativeNormal
         (False, _,      _, _) -> PositiveNormal

  compareByTotalOrder x y =
    let (x_hi, x_lo) = float128ToWord64Pair x
        (y_hi, y_lo) = float128ToWord64Pair y
    in compare (testBit y_hi 63) (testBit x_hi 63) -- sign bit
       <> if testBit x_hi 63 then
            compare y_hi x_hi <> compare y_lo x_lo -- negative
          else
            compare x_hi y_hi <> compare x_lo y_lo -- positive

{-# RULES
"nextUp/Float128" nextUp = nextUpF128
"nextDown/Float128" nextDown = nextDownF128
"nextTowardZero/Float128" nextTowardZero = nextTowardZeroF128
"isNormal/F128" isNormal = isNormalF128
"isFinite/F128" isFinite = isFiniteF128
"classify/F128" classify = classifyF128DiscardingSignalingNaNs
"isMantissaEven/F128"
  isMantissaEven = \x -> case x :: Float128 of F128 _hi lo -> even lo
"roundAway'/Float128" roundAway' = Numeric.Float128.round'
"ceiling'/Float128" ceiling' = Numeric.Float128.ceiling'
"floor'/Float128" floor' = Numeric.Float128.floor'
"truncate'/Float128" truncate' = Numeric.Float128.truncate'
  #-}

-- TODO: Write directly?
{-# SPECIALIZE minPositive :: Float128 #-}
{-# SPECIALIZE minPositiveNormal :: Float128 #-}
{-# SPECIALIZE maxFinite :: Float128 #-}

-- We shouldn't need specializations of positiveWordToBinaryFloatR# as long as WORD_SIZE_IN_BITS <= 113
{-# SPECIALIZE
  fromPositiveIntegerR :: RoundingStrategy f => Bool -> Integer -> f Float128
                        , Bool -> Integer -> RoundTiesToEven Float128
                        , Bool -> Integer -> RoundTiesToAway Float128
                        , Bool -> Integer -> RoundTowardPositive Float128
                        , Bool -> Integer -> RoundTowardNegative Float128
                        , Bool -> Integer -> RoundTowardZero Float128
  #-}
{-# SPECIALIZE
  fromPositiveRatioR :: RoundingStrategy f => Bool -> Integer -> Integer -> f Float128
                      , Bool -> Integer -> Integer -> RoundTiesToEven Float128
                      , Bool -> Integer -> Integer -> RoundTiesToAway Float128
                      , Bool -> Integer -> Integer -> RoundTowardPositive Float128
                      , Bool -> Integer -> Integer -> RoundTowardNegative Float128
                      , Bool -> Integer -> Integer -> RoundTowardZero Float128
  #-}
{-# SPECIALIZE
  encodePositiveFloatR# :: RoundingStrategy f => Bool -> Integer -> Int# -> f Float128
                         , Bool -> Integer -> Int# -> RoundTiesToEven Float128
                         , Bool -> Integer -> Int# -> RoundTiesToAway Float128
                         , Bool -> Integer -> Int# -> RoundTowardPositive Float128
                         , Bool -> Integer -> Int# -> RoundTowardNegative Float128
                         , Bool -> Integer -> Int# -> RoundTowardZero Float128
  #-}
{-# SPECIALIZE
  scaleFloatR# :: RoundingStrategy f => Int# -> Float128 -> f Float128
                , Int# -> Float128 -> RoundTiesToEven Float128
                , Int# -> Float128 -> RoundTiesToAway Float128
                , Int# -> Float128 -> RoundTowardPositive Float128
                , Int# -> Float128 -> RoundTowardNegative Float128
                , Int# -> Float128 -> RoundTowardZero Float128
  #-}
