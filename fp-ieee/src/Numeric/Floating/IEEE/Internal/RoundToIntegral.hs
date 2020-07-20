{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.RoundToIntegral where
import           MyPrelude

default ()

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Numeric.Floating.IEEE.Internal.Classify (isFinite)

-- |
-- IEEE 754 @roundToIntegralTiesToEven@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> (roundToIntegralTiesToEven x == fromInteger (round x))
-- prop> isNegativeZero (roundToIntegralTiesToEven (-0.5))
roundToIntegralTiesToEven :: RealFloat a => a -> a
roundToIntegralTiesToEven x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTiesToEven x = case round x of
                                0 | x < 0 -> -0
                                  | otherwise -> 0
                                n -> fromInteger n
{-# NOINLINE [1] roundToIntegralTiesToEven #-}

-- |
-- IEEE 754 @roundToIntegralTiesToAway@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> roundToIntegralTiesToAway x == fromInteger (roundAway x)
-- prop> isNegativeZero (roundToIntegralTiesToAway (-0.4))
roundToIntegralTiesToAway :: RealFloat a => a -> a
roundToIntegralTiesToAway x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTiesToAway x = case properFraction x of
                                -- x == n + f, signum x == signum f, 0 <= abs f < 1
                                (n,r) -> if abs r < 0.5 then
                                           -- round toward zero
                                           if n == 0 then
                                             0.0 * r -- signed zero
                                           else
                                             fromInteger n
                                         else
                                           -- round away from zero
                                           if r < 0 then
                                             fromInteger (n - 1)
                                           else
                                             fromInteger (n + 1)
{-# NOINLINE [1] roundToIntegralTiesToAway #-}

-- |
-- IEEE 754 @roundToIntegralTowardZero@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> roundToIntegralTowardZero x == fromInteger (truncate x)
-- prop> isNegativeZero (roundToIntegralTowardZero (-0.5))
roundToIntegralTowardZero :: RealFloat a => a -> a
roundToIntegralTowardZero x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTowardZero x = case truncate x of
                                0 | x < 0 -> -0
                                  | otherwise -> 0
                                n -> fromInteger n
{-# NOINLINE [1] roundToIntegralTowardZero #-}

-- |
-- IEEE 754 @roundToIntegralTowardPositive@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> roundToIntegralTowardPositive x == fromInteger (ceiling x)
-- prop> isNegativeZero (roundToIntegralTowardPositive (-0.5))
roundToIntegralTowardPositive :: RealFloat a => a -> a
roundToIntegralTowardPositive x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTowardPositive x = case ceiling x of
                                    0 | x < 0 -> -0
                                      | otherwise -> 0
                                    n -> fromInteger n
{-# NOINLINE [1] roundToIntegralTowardPositive #-}

-- |
-- IEEE 754 @roundToIntegralTowardNegative@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> roundToIntegralTowardNegative x == fromInteger (floor x)
-- prop> isNegativeZero (roundToIntegralTowardNegative (-0))
roundToIntegralTowardNegative :: RealFloat a => a -> a
roundToIntegralTowardNegative x | isInfinite x || isNaN x || isNegativeZero x = x
                                | otherwise = fromInteger (floor x)
{-# NOINLINE [1] roundToIntegralTowardNegative #-}

-- |
-- IEEE 754 @convertToIntegerTiesToAway@ operation.
--
-- >>> roundAway 4.5
-- 5
roundAway :: (RealFrac a, Integral b) => a -> b
roundAway x = case properFraction x of
                -- x == n + f, signum x == signum f, 0 <= abs f < 1
                (n,r) -> if abs r < 0.5 then
                           n
                         else
                           if r < 0 then
                             n - 1
                           else
                             n + 1
{-# INLINE roundAway #-}

#ifdef USE_FFI

foreign import ccall unsafe "ceilf"
  c_ceilFloat :: Float -> Float
foreign import ccall unsafe "ceil"
  c_ceilDouble :: Double -> Double
foreign import ccall unsafe "floorf"
  c_floorFloat :: Float -> Float
foreign import ccall unsafe "floor"
  c_floorDouble :: Double -> Double
foreign import ccall unsafe "roundf"
  c_roundFloat :: Float -> Float -- ties to away
foreign import ccall unsafe "round"
  c_roundDouble :: Double -> Double -- ties to away
foreign import ccall unsafe "truncf"
  c_truncFloat :: Float -> Float
foreign import ccall unsafe "floor"
  c_truncDouble :: Double -> Double

{-# RULES
"roundToIntegralTiesToAway/Float"
  roundToIntegralTiesToAway = c_roundFloat
"roundToIntegralTiesToAway/Double"
  roundToIntegralTiesToAway = c_roundDouble
"roundToIntegralTowardZero/Float"
  roundToIntegralTowardZero = c_truncFloat
"roundToIntegralTowardZero/Double"
  roundToIntegralTowardZero = c_truncDouble
"roundToIntegralTowardPositive/Float"
  roundToIntegralTowardPositive = c_ceilFloat
"roundToIntegralTowardPositive/Double"
  roundToIntegralTowardPositive = c_ceilDouble
"roundToIntegralTowardNegative/Float"
  roundToIntegralTowardNegative = c_floorFloat
"roundToIntegralTowardNegative/Double"
  roundToIntegralTowardNegative = c_floorDouble
  #-}

{- from base
foreign import ccall unsafe "rintFloat"
  c_rintFloat :: Float -> Float
foreign import ccall unsafe "rintDouble"
  c_rintDouble :: Double -> Double
-}
#if defined(HAS_FAST_ROUNDEVEN)
foreign import ccall unsafe "hs_roundevenFloat"
  c_roundevenFloat :: Float -> Float
foreign import ccall unsafe "hs_roundevenDouble"
  c_roundevenDouble :: Double -> Double

{-# RULES
"roundToIntegralTiesToEven/Float"
  roundToIntegralTiesToEven = c_roundevenFloat
"roundToIntegralTiesToEven/Double"
  roundToIntegralTiesToEven = c_roundevenDouble
  #-}
#endif

#endif
