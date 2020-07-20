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
-- prop> \(x :: Double) -> isFinite x ==> (round' x == fromInteger (round x))
-- >>> round' (-0.5)
-- -0.0
round' :: RealFloat a => a -> a
round' x | isInfinite x || isNaN x || isNegativeZero x = x + x
round' x = case round x of
             0 | x < 0 -> -0
               | otherwise -> 0
             n -> fromInteger n
{-# NOINLINE [1] round' #-}

-- |
-- IEEE 754 @roundToIntegralTiesToAway@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> roundAway' x == fromInteger (roundAway x)
-- >>> roundAway' (-0.5)
-- -1.0
-- >>> roundAway' (-0.4)
-- -0.0
roundAway' :: RealFloat a => a -> a
roundAway' x | isInfinite x || isNaN x || isNegativeZero x = x + x
roundAway' x = case properFraction x of
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
{-# NOINLINE [1] roundAway' #-}

-- |
-- IEEE 754 @roundToIntegralTowardZero@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> truncate' x == fromInteger (truncate x)
-- >>> truncate' (-0.5)
-- -0.0
truncate' :: RealFloat a => a -> a
truncate' x | isInfinite x || isNaN x || isNegativeZero x = x + x
truncate' x = case truncate x of
                0 | x < 0 -> -0
                  | otherwise -> 0
                n -> fromInteger n
{-# NOINLINE [1] truncate' #-}

-- |
-- IEEE 754 @roundToIntegralTowardPositive@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> ceiling' x == fromInteger (ceiling x)
-- >>> ceiling' (-0.8)
-- -0.0
-- >>> ceiling' (-0.5)
-- -0.0
ceiling' :: RealFloat a => a -> a
ceiling' x | isInfinite x || isNaN x || isNegativeZero x = x + x
ceiling' x = case ceiling x of
               0 | x < 0 -> -0
                 | otherwise -> 0
               n -> fromInteger n
{-# NOINLINE [1] ceiling' #-}

-- |
-- IEEE 754 @roundToIntegralTowardNegative@ operation.
--
-- prop> \(x :: Double) -> isFinite x ==> floor' x == fromInteger (floor x)
-- >>> floor' (-0.1)
-- -1.0
-- >>> floor' (-0)
-- -0.0
floor' :: RealFloat a => a -> a
floor' x | isInfinite x || isNaN x || isNegativeZero x = x + x
         | otherwise = fromInteger (floor x)
{-# NOINLINE [1] floor' #-}

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
"roundAway'/Float"
  roundAway' = c_roundFloat
"roundAway'/Double"
  roundAway' = c_roundDouble
"truncate'/Float"
  truncate' = c_truncFloat
"truncate'/Double"
  truncate' = c_truncDouble
"ceiling'/Float"
  ceiling' = c_ceilFloat
"ceiling'/Double"
  ceiling' = c_ceilDouble
"floor'/Float"
  floor' = c_floorFloat
"floor'/Double"
  floor' = c_floorDouble
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
"round'/Float"
  round' = c_roundevenFloat
"round'/Double"
  round' = c_roundevenDouble
  #-}
#endif

#endif
