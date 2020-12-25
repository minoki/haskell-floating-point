{-# LANGUAGE DataKinds #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Conversion (benchmark) where
import           Data.Bits
import           Data.Functor.Product
import           Data.Int
import           Data.Ratio
import           Data.Word
import           Gauge.Benchmark
import           Numeric.Floating.IEEE
import qualified Numeric.Floating.IEEE.Internal as IEEE.Internal
import           Numeric.Rounded.Hardware
import qualified Numeric.Rounded.Hardware.Backend.C as C
import           Numeric.Rounded.Hardware.Class
import           Numeric.Rounded.Hardware.Interval

word64ToDouble :: RoundingMode -> Word64 -> Double
word64ToDouble ToNearest x
  | x >= 0xFFFF_FFFF_FFFF_FC00 = 0x1p64
  | otherwise = let z = countLeadingZeros x
                    y = if x .&. (0x0000_0000_0000_0800 `unsafeShiftR` z) == 0
                        then x + (0x0000_0000_0000_03FF `unsafeShiftR` z)
                        else x + (0x0000_0000_0000_0400 `unsafeShiftR` z)
                in fromIntegral (y .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))
word64ToDouble TowardInf x
  | x >= 0xFFFF_FFFF_FFFF_F800 = 0x1p64
  | otherwise = let z = countLeadingZeros x
                    y = x + (0x0000_0000_0000_07FF `unsafeShiftR` z)
                in fromIntegral (y .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))
word64ToDouble TowardNegInf x = let z = countLeadingZeros x
                                in fromIntegral (x .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))
word64ToDouble TowardZero x = let z = countLeadingZeros x
                              in fromIntegral (x .&. (0xFFFF_FFFF_FFFF_F800 `unsafeShiftR` z))

int64ToDouble :: RoundingMode -> Int64 -> Double
int64ToDouble r x | x >= 0 = word64ToDouble r (fromIntegral x)
                  | r == TowardInf = - word64ToDouble TowardNegInf (fromIntegral (-x))
                  | r == TowardNegInf = - word64ToDouble TowardInf (fromIntegral (-x))
                  | otherwise = - word64ToDouble r (fromIntegral (-x))

benchmark :: Benchmark
benchmark = bgroup "Conversion"
  [ bgroup "fromInteger/to Double"
    [ bgroup name $ map ($ value)
      [ bench "plain" . nf (fromInteger :: Integer -> Double)
      , bench "Rounded/ToNearest" . nf (fromInteger :: Integer -> Rounded 'ToNearest Double)
      , bench "Rounded/TowardInf" . nf (fromInteger :: Integer -> Rounded 'TowardInf Double)
      , bench "roundedFromInteger/ToNearest" . nf (roundedFromInteger ToNearest :: Integer -> Double)
      , bench "roundedFromInteger/TowardInf" . nf (roundedFromInteger TowardInf :: Integer -> Double)
      , bench "fp-ieee/ToNearest" . nf (fromIntegerTiesToEven :: Integer -> Double)
      , bench "fp-ieee/TowardInf" . nf (fromIntegerTowardPositive :: Integer -> Double)
      , bench "Interval/default" . nf (fromInteger :: Integer -> Interval Double)
      , bench "Interval/individual" . nf (\n -> (fromIntegerTowardNegative n, fromIntegerTowardPositive n) :: (Double, Double))
      , bench "Interval/fromIntegerR" . nf (\n -> case IEEE.Internal.fromIntegerR n of
                                                    Pair (IEEE.Internal.RoundTowardNegative x) (IEEE.Internal.RoundTowardPositive y) -> (x, y) :: (Double, Double)
                                           )
      ]
    | (name, value) <- [ ("small", -2^50 + 2^13 + 127)
                       , ("medium", -2^60 + 42 * 2^53 - 137 * 2^24 + 3)
                       , ("large",  -2^100 - 37 * 2^80 + 2^13 + 127)
                       ] :: [(String, Integer)]
    ]
  , bgroup "fromIntegral/Int64->Double"
    [ bgroup name $ map ($ value)
      [ bench "plain" . nf (fromIntegral :: Int64 -> Double)
      , bench "Rounded/ToNearest" . nf (fromIntegral :: Int64 -> Rounded 'ToNearest Double)
      , bench "Rounded/TowardInf" . nf (fromIntegral :: Int64 -> Rounded 'TowardInf Double)
      , bench "roundedFromInteger/ToNearest" . nf (roundedFromInteger ToNearest . fromIntegral :: Int64 -> Double)
      , bench "roundedFromInteger/TowardInf" . nf (roundedFromInteger TowardInf . fromIntegral :: Int64 -> Double)
      , bench "fp-ieee/ToNearest" . nf (fromIntegralTiesToEven :: Int64 -> Double)
      , bench "fp-ieee/TowardInf" . nf (fromIntegralTowardPositive :: Int64 -> Double)
      , bench "int64ToDouble/ToNearest" . nf (int64ToDouble ToNearest :: Int64 -> Double)
      , bench "int64ToDouble/TowardInf" . nf (int64ToDouble TowardInf :: Int64 -> Double)
      , bench "Interval/default" . nf (fromIntegral :: Int64 -> Interval Double)
      , bench "Interval/individual" . nf (\n -> (fromIntegralTowardNegative n, fromIntegralTowardPositive n) :: (Double, Double))
      , bench "Interval/fromIntegralR" . nf (\n -> case IEEE.Internal.fromIntegralR n of
                                                Pair (IEEE.Internal.RoundTowardNegative x) (IEEE.Internal.RoundTowardPositive y) -> (x, y) :: (Double, Double)
                                            )
      , bench "Interval/individual/C" . nf (\n -> (C.roundedDoubleFromInt64 TowardNegInf n, C.roundedDoubleFromInt64 TowardInf n))
      ]
    | (name, value) <- [ ("small", -2^50 + 2^13 + 127)
                       , ("medium", -2^60 + 42 * 2^53 - 137 * 2^24 + 3)
                       ] :: [(String, Int64)]
    ]
  , bgroup "fromIntegral/Word64->Double"
    [ bgroup name $ map ($ value)
      [ bench "plain" . nf (fromIntegral :: Word64 -> Double)
      , bench "Rounded/ToNearest" . nf (fromIntegral :: Word64 -> Rounded 'ToNearest Double)
      , bench "Rounded/TowardInf" . nf (fromIntegral :: Word64 -> Rounded 'TowardInf Double)
      , bench "roundedFromInteger/ToNearest" . nf (roundedFromInteger ToNearest . fromIntegral :: Word64 -> Double)
      , bench "roundedFromInteger/TowardInf" . nf (roundedFromInteger TowardInf . fromIntegral :: Word64 -> Double)
      , bench "fp-ieee/ToNearest" . nf (fromIntegralTiesToEven :: Word64 -> Double)
      , bench "fp-ieee/TowardInf" . nf (fromIntegralTowardPositive :: Word64 -> Double)
      , bench "word64ToDouble/ToNearest" . nf (word64ToDouble ToNearest :: Word64 -> Double)
      , bench "word64ToDouble/TowardInf" . nf (word64ToDouble TowardInf :: Word64 -> Double)
      , bench "Interval/default" . nf (fromIntegral :: Word64 -> Interval Double)
      , bench "Interval/individual" . nf (\n -> (fromIntegralTowardNegative n, fromIntegralTowardPositive n) :: (Double, Double))
      , bench "Interval/fromIntegralR" . nf (\n -> case IEEE.Internal.fromIntegralR n of
                                                Pair (IEEE.Internal.RoundTowardNegative x) (IEEE.Internal.RoundTowardPositive y) -> (x, y) :: (Double, Double)
                                            )
      , bench "Interval/individual/C" . nf (\n -> (C.roundedDoubleFromWord64 TowardNegInf n, C.roundedDoubleFromWord64 TowardInf n))
      ]
    | (name, value) <- [ ("small", 2^50 + 2^13 + 127)
                       , ("medium", 2^63 + 42 * 2^53 - 137 * 2^24 + 3)
                       ] :: [(String, Word64)]
    ]
  , bgroup "fromRational/to Double"
    [ bgroup name $ map ($ value)
      [ bench "plain" . nf (fromRational :: Rational -> Double)
      , bench "Rounded/ToNearest" . nf (fromRational :: Rational -> Rounded 'ToNearest Double)
      , bench "Rounded/TowardInf" . nf (fromRational :: Rational -> Rounded 'TowardInf Double)
      , bench "fp-ieee/ToNearest" . nf (fromRationalTiesToEven :: Rational -> Double)
      , bench "fp-ieee/TowardInf" . nf (fromRationalTowardPositive :: Rational -> Double)
      , bench "Interval/default" . nf (fromRational :: Rational -> Interval Double)
      , bench "Interval/individual" . nf (\x -> (fromRationalTowardNegative x :: Double, fromRationalTowardPositive x :: Double))
      , bench "Interval/fromRationalR" . nf (\x -> case IEEE.Internal.fromRationalR x of
                                                     Pair (IEEE.Internal.RoundTowardNegative a) (IEEE.Internal.RoundTowardPositive b) -> (a, b) :: (Double, Double)
                                            )
      ]
    | (name, value) <- [ ("decimal", 3.14159265358979323846264338327950)
                       , ("binary", 0xcafec0ffeecafec0ffeep-177)
                       , ("small", 22 % 7)
                       , ("large", 78326489123342523452342137498719847192 % 348912374981749170413424213275017)
                       ] :: [(String, Rational)]
    ]
  ]
