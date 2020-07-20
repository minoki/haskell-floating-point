{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Floating.IEEE.Internal.Augmented where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Ratio
import           Math.NumberTheory.Logarithms (integerLog2')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Classify
import           Numeric.Floating.IEEE.Internal.FMA
import           Numeric.Floating.IEEE.Internal.NextFloat

default ()

-- |
-- IEEE 754 @augmentedAddition@ operation.
augmentedAddition :: RealFloat a => a -> a -> (a, a)
augmentedAddition !x !y
  | isNaN x || isInfinite x || isNaN y || isInfinite y = let !result = x + y in (result, result)
  | otherwise = let (u1, u2) = twoSum x y
                    ulpTowardZero = u1 - nextTowardZero u1
                in if isInfinite u1 then
                     -- Handle undue overflow: e.g. 0x1.ffff_ffff_ffff_f8p1023
                     handleUndueOverflow
                   else
                     if u2 == 0 then
                       (u1, 0 * u1) -- signed zero
                     else
                       if (-2) * u2 == ulpTowardZero then
                         (u1 - ulpTowardZero, ulpTowardZero + u2)
                       else
                         (u1, u2)
  where
    handleUndueOverflow =
      let e = max (exponent x) (exponent y)
          x' = scaleFloat (- e) x
          y' = scaleFloat (- e) y
          (u1, u2) = twoSum x' y'
          ulpTowardZero = u1 - nextTowardZero u1
          (v1, v2) | (-2) * u2 == ulpTowardZero = (u1 - ulpTowardZero, ulpTowardZero + u2)
                   | otherwise = (u1, u2)
          r1 = scaleFloat e v1
          r2 = scaleFloat e v2
      in if isInfinite r1 then
           (r1, r1) -- unavoidable overflow
         else
           assert (r2 /= 0) (r1, r2)
{-# SPECIALIZE augmentedAddition :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

-- |
-- IEEE 754 @augmentedSubtraction@ operation.
augmentedSubtraction :: RealFloat a => a -> a -> (a, a)
augmentedSubtraction x y = augmentedAddition x (negate y)

-- |
-- IEEE 754 @augmentedMultiplication@ operation.
augmentedMultiplication :: RealFloat a => a -> a -> (a, a)
augmentedMultiplication !x !y
  | isNaN x || isInfinite x || isNaN y || isInfinite y || x * y == 0 = let !result = x * y in (result, result)
  | otherwise = let exy = exponent x + exponent y
                    x' = significand x
                    y' = significand y
                    (u1, u2) = twoProduct_nonscaling x' y'
                    !_ = assert (toRational x' * toRational y' == toRational u1 + toRational u2) ()
                    -- The product is subnormal <=> exy + exponent u1 < expMin
                    -- The product is inexact => exy + exponent u1 < expMin + d
                in if exy + exponent u1 >= expMin then
                     -- The result is exact
                     let ulpTowardZero = u1 - nextTowardZero u1
                         !_ = assert (2 * abs u2 <= abs ulpTowardZero) ()
                         (v1, v2) = if (-2) * u2 == ulpTowardZero then
                                      (u1 - ulpTowardZero, ulpTowardZero + u2)
                                    else
                                      (u1, u2)
                         !_ = assert (v1 + v2 == u1 + u2) ()
                         r1 = scaleFloat exy v1
                         !_ = assert (r1 == roundTiesTowardZero (toRational x * toRational y)) ()
                     in if isInfinite r1 then
                          (r1, r1)
                        else
                          if v2 == 0 then
                            (r1, 0 * r1) -- signed zero
                          else
                            if exy >= expMin + d then
                              -- The result is exact
                              let r2 = scaleFloat exy v2
                              in (r1, r2)
                            else
                              -- The upper part is normal, the lower is subnormal (and inexact)
                              -- Compute 'scaleFloat exy v2' with roundTiesTowardZero
                              let !r2 = scaleFloatIntoSubnormalTiesTowardZero exy v2
                                  !_ = assert (r2 == roundTiesTowardZero (toRational x * toRational y - toRational r1)) ()
                              in (r1, r2)
                   else
                     -- The upper part is subnormal (possibly inexact), and the lower is signed zero (possibly inexact)
                     if u2 == 0 then
                       -- u1 is exact
                       let !_ = assert (toRational x' * toRational y' == toRational u1) ()
                           r1 = scaleFloatIntoSubnormalTiesTowardZero exy u1
                           r1' = scaleFloat (-exy) r1
                       in if u1 == r1' then
                            (r1, 0 * r1)
                          else
                            (r1, 0 * (u1 - r1'))
                     else
                       let u1' = scaleFloat exy u1
                           v1' = scaleFloat exy (if u2 > 0 then nextUp u1 else nextDown u1)
                           r1 = if u1' == v1' || not (isMantissaEven u1') then
                                  u1'
                                else
                                  v1'
                           r1' = scaleFloat (-exy) r1
                       in (r1, 0 * (u1 - r1' + u2))
  where
    d = floatDigits x
    (expMin,_expMax) = floatRange x

    -- Compute 'scaleFloat e z' with roundTiesTowardZero
    scaleFloatIntoSubnormalTiesTowardZero e z =
      let z' = scaleFloat e z
          w' = scaleFloat e (nextTowardZero z)
      in if z' == w' || not (isMantissaEven z') then
           z'
         else
           w'
{-# SPECIALIZE augmentedMultiplication :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

augmentedAddition_viaRational :: (RealFloat a, Show a) => a -> a -> (a, a)
augmentedAddition_viaRational x y
  | isFinite x && isFinite y && (x /= 0 || y /= 0) =
    let z :: Rational
        z = toRational x + toRational y
        z' = roundTiesTowardZero z `asTypeOf` x
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w = z - toRational z'
             w' = roundTiesTowardZero w `asTypeOf` x
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x + y
                in (z, z)

augmentedMultiplication_viaRational :: (RealFloat a, Show a) => a -> a -> (a, a)
augmentedMultiplication_viaRational x y
  | isFinite x && isFinite y && x * y /= 0 =
    let z :: Rational
        z = toRational x * toRational y
        z' = roundTiesTowardZero z `asTypeOf` x
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w = z - toRational z'
             w' = roundTiesTowardZero w `asTypeOf` x
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x * y
                in (z, z)

roundTiesTowardZero :: forall a. RealFloat a => Rational -> a
roundTiesTowardZero x | x < 0 = - fromPositiveRatio (- numerator x) (denominator x)
                      | otherwise = fromPositiveRatio (numerator x) (denominator x)
  where
    fromPositiveRatio :: Integer -> Integer -> a
    fromPositiveRatio !n !d
      = let ln, ld, e :: Int
            ln = integerLog2' n
            ld = integerLog2' d
            e = ln - ld - fDigits
            q, r, d_ :: Integer
            d_ | e >= 0 = d `unsafeShiftL` e
               | otherwise = d
            (!q, !r) | e >= 0 = n `quotRem` d_
                     | otherwise = (n `unsafeShiftL` (-e)) `quotRem` d
            !_ = assert (n % d * 2^^(-e) == fromInteger q + r % d_) ()
            -- e >= 0: n = q * (d * 2^e) + r, 0 <= r < d * 2^e
            -- e <= 0: n * 2^(-e) = q * d + r, 0 <= r < d
            -- n / d * 2^^(-e) = q + r / d_
            -- 52 <= log2 q < 54

            q', r', d' :: Integer
            e' :: Int
            (!q', !r', !d', !e') | q < (1 `unsafeShiftL` fDigits) = (q, r, d_, e)
                                 | otherwise = let (q'', r'') = q `quotRem` 2
                                               in (q'', r'' * d_ + r, 2 * d_, e + 1)
            !_ = assert (n % d * 2^^(-e') == fromInteger q' + r' % d') ()
            -- n / d * 2^^(-e') = q' + r' / d', 2^52 <= q' < 2^53, 0 <= r' < d'
            -- q' * 2^^e' <= n/d < (q'+1) * 2^^e', 2^52 <= q' < 2^53
            -- (q'/2^53) * 2^^(e'+53) <= n/d < (q'+1)/2^53 * 2^^(e'+53), 1/2 <= q'/2^53 < 1
            -- normal: 0x1p-1022 <= x <= 0x1.fffffffffffffp+1023
      in if expMin <= e' + fDigits && e' + fDigits <= expMax then
           -- normal
           if r' == 0 then
             encodeFloat q' e' -- exact
           else
             -- inexact
             let down = encodeFloat q' e'
                 up = encodeFloat (q' + 1) e' -- may be infinity
                 toNearest = case compare (2 * r') d' of
                   LT -> down
                   EQ -> down -- ties toward zero
                   GT -> up
             in toNearest
         else
           -- infinity or subnormal
           if expMax <= e' + fDigits then
             -- infinity
             (1 / 0) -- ToNearest
           else
             -- subnormal
             -- e' + fDigits < expMin (or, e' < expMin - fDigits = -1074)
             -- 0 <= rounded(n/d) <= 2^(expMin - 1) = 0x1p-1022, minimum (positive) subnormal: 0x1p-1074
             let (!q'', !r'') = q' `quotRem` (1 `unsafeShiftL` (expMin - fDigits - e'))
                 -- q' = q'' * 2^(expMin - fDigits - e') + r'', 0 <= r'' < 2^(expMin - fDigits - e')
                 -- 2^(fDigits-1) <= q' = q'' * 2^(expMin - fDigits - e') + r'' < 2^fDigits
                 -- n / d * 2^^(-e') = q' + r' / d' = q'' * 2^(expMin - fDigits - e') + r'' + r' / d'
                 -- n / d = q'' * 2^^(expMin - fDigits) + (r'' + r' / d') * 2^^e'
                 -- 0 <= r'' < 2^(expMin - fDigits - e')
             in if r' == 0 && r'' == 0 then
                  encodeFloat q'' (expMin - fDigits) -- exact
                else
                  let down = encodeFloat q'' (expMin - fDigits)
                      up = encodeFloat (q'' + 1) (expMin - fDigits)
                      toNearest = case compare r'' (1 `unsafeShiftL` (expMin - fDigits - e' - 1)) of
                                    LT -> down
                                    GT -> up
                                    EQ | r' /= 0   -> up
                                       | otherwise -> down -- ties toward zero
                  in toNearest
      where
        !fDigits = floatDigits (undefined :: a) -- 53 for Double
        (!expMin, !expMax) = floatRange (undefined :: a) -- (-1021, 1024) for Double
