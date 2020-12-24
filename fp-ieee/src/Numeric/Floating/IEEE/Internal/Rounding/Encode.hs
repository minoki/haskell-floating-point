{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Floating.IEEE.Internal.Rounding.Encode where
import           Control.Exception (assert)
import           Data.Functor.Product
import           Data.Int
import           GHC.Exts
import           Math.NumberTheory.Logarithms (integerLog2', integerLogBase')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.Classify (isFinite)
import           Numeric.Floating.IEEE.Internal.Rounding.Common

default ()

encodeFloatTiesToEven, encodeFloatTiesToAway, encodeFloatTowardPositive, encodeFloatTowardNegative, encodeFloatTowardZero :: RealFloat a => Integer -> Int -> a
encodeFloatTiesToEven m = roundTiesToEven . encodeFloatR m
encodeFloatTiesToAway m = roundTiesToAway . encodeFloatR m
encodeFloatTowardPositive m = roundTowardPositive . encodeFloatR m
encodeFloatTowardNegative m = roundTowardNegative . encodeFloatR m
encodeFloatTowardZero m = roundTowardZero . encodeFloatR m
{-# INLINE encodeFloatTiesToEven #-}
{-# INLINE encodeFloatTiesToAway #-}
{-# INLINE encodeFloatTowardPositive #-}
{-# INLINE encodeFloatTowardNegative #-}
{-# INLINE encodeFloatTowardZero #-}

encodeFloatR :: (RealFloat a, RoundingStrategy f) => Integer -> Int -> f a
encodeFloatR 0 !_ = exact 0
encodeFloatR m n | m < 0 = negate <$> encodePositiveFloatR True (- m) n
                 | otherwise = encodePositiveFloatR False m n
{-# INLINE encodeFloatR #-}

-- Avoid cross-module specialization issue with manual worker/wrapper transformation
encodePositiveFloatR :: (RealFloat a, RoundingStrategy f) => Bool -> Integer -> Int -> f a
encodePositiveFloatR neg m (I# n#) = encodePositiveFloatR# neg m n#
{-# INLINE encodePositiveFloatR #-}

encodePositiveFloatR# :: forall f a. (RealFloat a, RoundingStrategy f) => Bool -> Integer -> Int# -> f a
encodePositiveFloatR# !neg !m n# = assert (m > 0) result
  where
    n = I# n#
    result = let k = if base == 2 then
                       integerLog2' m
                     else
                       integerLogBase' base m
                 -- base^k <= m < base^(k+1)
                 -- base^^(k+n) <= m * base^^n < base^^(k+n+1)
             in if expMin <= k + n + 1 && k + n + 1 <= expMax then
                  -- normal
                  -- base^(fDigits-1) <= m / base^(k-fDigits+1) < base^fDigits
                  if k < fDigits then
                    -- m < base^(k+1) <= base^fDigits
                    exact $ encodeFloat m n
                  else
                    -- k >= fDigits
                    let (q,r) = quotRemByExpt m base (k - fDigits + 1)
                        -- m = q * base^^(k-fDigits+1) + r
                        -- base^(fDigits-1) <= q = m `quot` (base^^(k-fDigits+1)) < base^fDigits
                        -- m * base^^n = q * base^^(n+k-fDigits+1) + r * base^^n
                        towardzero_or_exact = encodeFloat q (n + k - fDigits + 1)
                        awayfromzero = encodeFloat (q + 1) (n + k - fDigits + 1)
                        parity = fromInteger q :: Int
                    in doRound
                         (isDivisibleByExpt m base (k - fDigits + 1) r) -- exactness (r == 0)
                         (compareWithExpt base m r (k - fDigits))
                         -- (compare r (expt base (k - fDigits)))
                         neg
                         parity
                         towardzero_or_exact
                         awayfromzero
                else
                  if expMax <= k + n then
                    -- overflow
                    let inf = 1 / 0
                    in inexact GT neg 1 maxFinite inf
                  else -- k + n + 1 < expMin
                    -- subnormal
                    if expMin - fDigits <= n then
                      -- k <= expMin - n <= fDigits
                      exact $ encodeFloat m n
                    else -- n < expMin - fDigits
                      -- k <= expMin - n, fDigits < expMin - n
                      let (q,r) = quotRemByExpt m base (expMin - fDigits - n)
                          -- m = q * base^(expMin-fDigits-n) + r
                          -- q <= m * base^^(n-expMin+fDigits) < q+1
                          -- q * base^^(expMin-fDigits) <= m * base^^n < (q+1) * base^^(expMin-fDigits)
                          !_ = assert (toRational q * toRational base^^(expMin-fDigits) <= toRational m * toRational base^^n) ()
                          !_ = assert (toRational m * toRational base^^n < toRational (q+1) * toRational base^^(expMin-fDigits)) ()
                          towardzero_or_exact = encodeFloat q (expMin - fDigits)
                          awayfromzero = encodeFloat (q + 1) (expMin - fDigits)
                          parity = fromInteger q :: Int
                      in doRound
                           (isDivisibleByExpt m base (expMin - fDigits - n) r) -- exactness (r == 0)
                           (compareWithExpt base m r (expMin - fDigits - n - 1))
                           -- (compare r (expt base (expMin - fDigits - n - 1)))
                           neg
                           parity
                           towardzero_or_exact
                           awayfromzero

    !base = floatRadix (undefined :: a)
    !fDigits = floatDigits (undefined :: a) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined :: a) -- (-1021, 1024) for Double
{-# INLINABLE [0] encodePositiveFloatR# #-}
{-# SPECIALIZE
  encodePositiveFloatR# :: RealFloat a => Bool -> Integer -> Int# -> RoundTiesToEven a
                         , RealFloat a => Bool -> Integer -> Int# -> RoundTiesToAway a
                         , RealFloat a => Bool -> Integer -> Int# -> RoundTowardPositive a
                         , RealFloat a => Bool -> Integer -> Int# -> RoundTowardZero a
                         , RealFloat a => Bool -> Integer -> Int# -> Product RoundTowardNegative RoundTowardPositive a
                         , RoundingStrategy f => Bool -> Integer -> Int# -> f Double
                         , RoundingStrategy f => Bool -> Integer -> Int# -> f Float
                         , Bool -> Integer -> Int# -> RoundTiesToEven Double
                         , Bool -> Integer -> Int# -> RoundTiesToAway Double
                         , Bool -> Integer -> Int# -> RoundTowardPositive Double
                         , Bool -> Integer -> Int# -> RoundTowardZero Double
                         , Bool -> Integer -> Int# -> RoundTiesToEven Float
                         , Bool -> Integer -> Int# -> RoundTiesToAway Float
                         , Bool -> Integer -> Int# -> RoundTowardPositive Float
                         , Bool -> Integer -> Int# -> RoundTowardZero Float
                         , Bool -> Integer -> Int# -> Product RoundTowardNegative RoundTowardPositive Double
                         , Bool -> Integer -> Int# -> Product RoundTowardNegative RoundTowardPositive Float
  #-}
{-# RULES
"encodePositiveFloatR#/RoundTowardNegative"
  encodePositiveFloatR# = \neg x y -> RoundTowardNegative (roundTowardPositive (encodePositiveFloatR# (not neg) x y))
  #-}

-- |
-- IEEE 754 @scaleB@ operation, with each rounding attributes.
scaleFloatTiesToEven, scaleFloatTiesToAway, scaleFloatTowardPositive, scaleFloatTowardNegative, scaleFloatTowardZero :: RealFloat a => Int -> a -> a
scaleFloatTiesToEven e = roundTiesToEven . scaleFloatR e
scaleFloatTiesToAway e = roundTiesToAway . scaleFloatR e
scaleFloatTowardPositive e = roundTowardPositive . scaleFloatR e
scaleFloatTowardNegative e = roundTowardNegative . scaleFloatR e
scaleFloatTowardZero e = roundTowardZero . scaleFloatR e
{-# INLINE scaleFloatTiesToEven #-}
{-# INLINE scaleFloatTiesToAway #-}
{-# INLINE scaleFloatTowardPositive #-}
{-# INLINE scaleFloatTowardNegative #-}
{-# INLINE scaleFloatTowardZero #-}

scaleFloatR :: (RealFloat a, RoundingStrategy f) => Int -> a -> f a
scaleFloatR (I# e#) x = scaleFloatR# e# x
{-# INLINE scaleFloatR #-}

scaleFloatR# :: (RealFloat a, RoundingStrategy f) => Int# -> a -> f a
scaleFloatR# e# x
  | x /= 0, isFinite x =
      let e = I# e#
          (m,n) = decodeFloat x
          -- x = m * base^^n, expMin <= n <= expMax
          -- base^(fDigits-1) <= abs m < base^fDigits
          -- base^(fDigits+n+e-1) <= abs x * base^^e < base^(fDigits+n+e)
      in if expMin - fDigits <= n + e && n + e <= expMax - fDigits then
           -- normal
           exact $ encodeFloat m (n + e)
         else
           if expMax - fDigits < n + e then
             -- infinity
             (signum x *) <$> inexact GT (x < 0) 1 maxFinite (1 / 0)
           else
             -- subnormal
             let !_ = assert (e + n < expMin - fDigits) ()
                 m' = abs m
                 (q,r) = quotRemByExpt m' base (expMin - fDigits - (e + n))
                 towardzero_or_exact = encodeFloat q (expMin - fDigits)
                 awayfromzero = encodeFloat (q + 1) (expMin - fDigits)
                 parity = fromInteger q :: Int
             in (signum x *) <$> doRound
                  (isDivisibleByExpt m' base (expMin - fDigits - (e + n)) r)
                  (compareWithExpt base m' r (expMin - fDigits - (e + n) - 1))
                  (x < 0)
                  parity
                  towardzero_or_exact
                  awayfromzero
  | otherwise = exact (x + x) -- +-0, +-Infinity, NaN
  where
    base = floatRadix x
    (expMin,expMax) = floatRange x
    fDigits = floatDigits x
{-# INLINABLE [0] scaleFloatR# #-}
{-# SPECIALIZE
  scaleFloatR# :: RealFloat a => Int# -> a -> RoundTiesToEven a
                , RealFloat a => Int# -> a -> RoundTiesToAway a
                , RealFloat a => Int# -> a -> RoundTowardPositive a
                , RealFloat a => Int# -> a -> RoundTowardNegative a
                , RealFloat a => Int# -> a -> RoundTowardZero a
                , RoundingStrategy f => Int# -> Double -> f Double
                , RoundingStrategy f => Int# -> Float -> f Float
                , Int# -> Double -> RoundTiesToEven Double
                , Int# -> Double -> RoundTiesToAway Double
                , Int# -> Double -> RoundTowardPositive Double
                , Int# -> Double -> RoundTowardNegative Double
                , Int# -> Double -> RoundTowardZero Double
                , Int# -> Float -> RoundTiesToEven Float
                , Int# -> Float -> RoundTiesToAway Float
                , Int# -> Float -> RoundTowardPositive Float
                , Int# -> Float -> RoundTowardNegative Float
                , Int# -> Float -> RoundTowardZero Float
  #-}
