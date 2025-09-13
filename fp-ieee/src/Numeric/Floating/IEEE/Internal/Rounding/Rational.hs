{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Floating.IEEE.Internal.Rounding.Rational where
import           Control.Exception (assert)
import           Data.Functor.Product
import           Data.Ratio
import           GHC.Float (expt)
import           Math.NumberTheory.Logarithms (integerLog2', integerLogBase')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.Rounding.Common

default ()

-- |
-- Conversion from a rational number to floating-point value, with each rounding attributes.
fromRationalTiesToEven, fromRationalTiesToAway, fromRationalTowardPositive, fromRationalTowardNegative, fromRationalTowardZero :: RealFloat a => Rational -> a
fromRationalTiesToEven = roundTiesToEven . fromRationalR
fromRationalTiesToAway = roundTiesToAway . fromRationalR
fromRationalTowardPositive = roundTowardPositive . fromRationalR
fromRationalTowardNegative = roundTowardNegative . fromRationalR
fromRationalTowardZero = roundTowardZero . fromRationalR
{-# INLINE fromRationalTiesToEven #-}
{-# INLINE fromRationalTiesToAway #-}
{-# INLINE fromRationalTowardPositive #-}
{-# INLINE fromRationalTowardNegative #-}
{-# INLINE fromRationalTowardZero #-}

fromRationalR :: (RealFloat a, RoundingStrategy f) => Rational -> f a
fromRationalR x = fromRatioR (numerator x) (denominator x)
{-# INLINE fromRationalR #-}

fromRatioR :: (RealFloat a, RoundingStrategy f)
           => Integer -- ^ numerator
           -> Integer -- ^ denominator
           -> f a
fromRatioR 0 !_ = exact 0
fromRatioR n 0 | n > 0 = exact (1 / 0) -- positive infinity
               | otherwise = exact (- 1 / 0) -- negative infinity
fromRatioR n d | d < 0 = error "fromRatio: negative denominator"
               | n < 0 = negate <$> fromPositiveRatioR True (- n) d
               | otherwise = fromPositiveRatioR False n d
{-# INLINE fromRatioR #-}

fromPositiveRatioR :: forall f a. (RealFloat a, RoundingStrategy f)
                   => Bool -- ^ True if the result will be negated
                   -> Integer -- ^ numerator (> 0)
                   -> Integer -- ^ denominator (> 0)
                   -> f a
fromPositiveRatioR !neg !n !d = assert (n > 0 && d > 0) result
  where
    result = let e0 :: Int
                 e0 = if base == 2 then
                        integerLog2' n - integerLog2' d - fDigits
                      else
                        integerLogBase' base n - integerLogBase' base d - fDigits
                 q0, r0, d0 :: Integer
                 (!d0, (!q0, !r0)) =
                   if e0 >= 0 then
                     -- n = q0 * (d * base^e0) + r0, 0 <= r0 < d * base^e0
                     let d_ = multiplyByExpt d base e0
                     in (d_, n `quotRem` d_)
                   else
                     -- n * base^(-e0) = q0 * d + r0, 0 <= r0 < d
                     (d, (multiplyByExpt n base (-e0)) `quotRem` d)
                 -- Invariant: n / d * base^^(-e0) = q0 + r0 / d0
                 !_ = assert (n % d * fromInteger base^^(-e0) == fromInteger q0 + r0 % d0) ()
                 !_ = assert (base^(fDigits-1) <= q0 && q0 < base^(fDigits+1)) ()

                 q, r, d' :: Integer
                 e :: Int
                 (!q, !r, !d', !e) =
                   if q0 < expt base fDigits then
                     -- base^(fDigits-1) <= q0 < base^fDigits
                     (q0, r0, d0, e0)
                   else
                     -- base^fDigits <= q0 < base^(fDigits+1)
                     let (q', r') = q0 `quotRem` base
                     in (q', r' * d0 + r0, base * d0, e0 + 1)
                 -- Invariant: n / d * 2^^(-e) = q + r / d', base^(fDigits-1) <= q < base^fDigits, 0 <= r < d'
                 !_ = assert (n % d * fromInteger base^^(-e) == fromInteger q + r % d') ()
                 -- base^(e+fDigits-1) <= q * base^^e <= n/d < (q+1) * base^^e <= base^(e+fDigits)
                 -- In particular, base^(fDigits-1) <= q < base^fDigits
             in if expMin <= e + fDigits && e + fDigits <= expMax then
                  -- normal: base^^(expMin-1) <= n/d < base^expMax
                  let towardzero_or_exact = encodeFloat q e
                      awayfromzero = encodeFloat (q + 1) e -- may be infinity
                      parity = fromInteger q :: Int
                  in doRound
                       (r == 0)
                       (compare (base * r) d')
                       neg
                       parity
                       towardzero_or_exact
                       awayfromzero
                else
                  if expMax < e + fDigits then
                    -- overflow
                    let inf = 1 / 0
                    in inexact GT neg 1 maxFinite inf
                  else
                    -- subnormal: 0 < n/d < base^^(expMin-1)
                    -- e + fDigits < expMin
                    let (q', r') = quotRemByExpt q base (expMin - fDigits - e)
                        !_ = assert (q == q' * base^(expMin-fDigits-e) + r' && 0 <= r' && r' < base^(expMin-fDigits-e)) ()
                        -- q = q' * base^(expMin-fDigits-e) + r', 0 <= r' < base^(expMin-fDigits-e)
                        -- n / d * base^^(-e) = q' * base^(expMin-fDigits-e) + r' + r / d'
                        -- n / d = q' * base^^(expMin - fDigits) + (r' + r / d') * base^^e
                        !_ = assert (n % d == fromInteger q' * fromInteger base^^(expMin - fDigits) + (fromInteger r' + r % d') * fromInteger base^^e) ()
                        -- rounding direction: (r' + r / d') * base^^e vs. base^^(expMin-fDigits-1)
                        towardzero = encodeFloat q' (expMin - fDigits)
                        awayfromzero = encodeFloat (q' + 1) (expMin - fDigits)
                        parity = fromInteger q' :: Int
                    in doRound
                         (r == 0 && r' == 0)
                         (compareWithExpt base q r' (expMin - fDigits - e - 1) <> if r == 0 then EQ else GT)
                         -- (compare r' (expt base (expMin - fDigits - e - 1)) <> if r == 0 then EQ else GT)
                         neg
                         parity
                         towardzero
                         awayfromzero

    !base = floatRadix (undefined :: a)
    !fDigits = floatDigits (undefined :: a) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined :: a) -- (-1021, 1024) for Double
{-# INLINABLE [0] fromPositiveRatioR #-}
{-# SPECIALIZE fromPositiveRatioR :: RealFloat a => Bool -> Integer -> Integer -> RoundTiesToEven a #-}
{-# SPECIALIZE fromPositiveRatioR :: RealFloat a => Bool -> Integer -> Integer -> RoundTiesToAway a #-}
{-# SPECIALIZE fromPositiveRatioR :: RealFloat a => Bool -> Integer -> Integer -> RoundTowardPositive a #-}
{-# SPECIALIZE fromPositiveRatioR :: RealFloat a => Bool -> Integer -> Integer -> RoundTowardZero a #-}
{-# SPECIALIZE fromPositiveRatioR :: RealFloat a => Bool -> Integer -> Integer -> Product RoundTowardNegative RoundTowardPositive a #-}
{-# SPECIALIZE fromPositiveRatioR :: RoundingStrategy f => Bool -> Integer -> Integer -> f Double #-}
{-# SPECIALIZE fromPositiveRatioR :: RoundingStrategy f => Bool -> Integer -> Integer -> f Float #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTiesToEven Double #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTiesToAway Double #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTowardPositive Double #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTowardZero Double #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTiesToEven Float #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTiesToAway Float #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTowardPositive Float #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> RoundTowardZero Float #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> Product RoundTowardNegative RoundTowardPositive Double #-}
{-# SPECIALIZE fromPositiveRatioR :: Bool -> Integer -> Integer -> Product RoundTowardNegative RoundTowardPositive Float #-}
{-# RULES
"fromPositiveRatioR/RoundTowardNegative"
  fromPositiveRatioR = \neg x y -> RoundTowardNegative (roundTowardPositive (fromPositiveRatioR (not neg) x y))
  #-}
