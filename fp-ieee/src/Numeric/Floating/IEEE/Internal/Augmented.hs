{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Augmented where
import           Control.Exception (assert)
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.FMA (isMantissaEven,
                                                     twoProduct_nonscaling,
                                                     twoSum)
import           Numeric.Floating.IEEE.Internal.NextFloat (nextDown,
                                                           nextTowardZero,
                                                           nextUp)

default ()

-- $setup
-- >>> :set -XDeriveFunctor
-- >>> import Numeric.Floating.IEEE.Internal.Augmented
-- >>> import Numeric.Floating.IEEE.Internal.Rounding.Common
-- >>> import Numeric.Floating.IEEE.Internal.Rounding.Rational
-- >>> :{
-- newtype RoundTiesTowardZero a = RoundTiesTowardZero { roundTiesTowardZero :: a }
--   deriving (Functor)
-- instance RoundingStrategy RoundTiesTowardZero where
--   exact = RoundTiesTowardZero
--   inexact o _neg _parity zero away = RoundTiesTowardZero $ case o of
--                                                              LT -> zero
--                                                              EQ -> zero
--                                                              GT -> away
--   doRound _exact o _neg _parity zero away = RoundTiesTowardZero $ case o of
--     LT -> zero
--     EQ -> zero
--     GT -> away
-- :}

-- |
-- IEEE 754 @augmentedAddition@ operation.
--
-- The first return value is the approximation of the sum, and the second return value is the error.
--
-- prop> fst (augmentedAddition x y) == roundTiesTowardZero (fromRationalR (toRational x + toRational y)) `const` (x :: Double)
-- prop> let (u, v) = augmentedAddition x y in toRational u + toRational v == toRational x + toRational y `const` (x :: Double)
augmentedAddition :: RealFloat a => a -> a -> (a, a)
augmentedAddition !x !y
  | isNaN x || isInfinite x || isNaN y || isInfinite y = let !result = x + y in (result, result)
  | otherwise = let (u1, u2) = twoSum x y
                    ulpTowardZero = u1 - nextTowardZero u1
                in if isNaN u2 then
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
      -- The exponents of inputs should be close enough so that neither x' nor y' underflow.
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
{-# SPECIALIZE augmentedAddition :: Float -> Float -> (Float, Float) #-}
{-# SPECIALIZE augmentedAddition :: Double -> Double -> (Double, Double) #-}

-- |
-- IEEE 754 @augmentedSubtraction@ operation.
--
-- The first return value is the approximation of the difference, and the second return value is the error.
--
-- prop> fst (augmentedSubtraction x y) == roundTiesTowardZero (fromRationalR (toRational x - toRational y)) `const` (x :: Double)
-- prop> let (u, v) = augmentedSubtraction x y in toRational u + toRational v == toRational x - toRational y `const` (x :: Double)
augmentedSubtraction :: RealFloat a => a -> a -> (a, a)
augmentedSubtraction x y = augmentedAddition x (negate y)

-- |
-- IEEE 754 @augmentedMultiplication@ operation.
--
-- The first return value is the approximation of the product, and the second return value is the error.
--
-- prop> fst (augmentedMultiplication x y) == roundTiesTowardZero (fromRationalR (toRational x * toRational y)) `const` (x :: Double)
-- prop> let (u, v) = augmentedMultiplication x y in toRational u + toRational v == toRational x * toRational y `const` (x :: Double)
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
                         !_ = assert (case u1 of
                                        0.5  -> - ulpTowardZero <= 2 * u2 && u2 <= ulpTowardZero
                                        -0.5 -> ulpTowardZero <= u2 && 2 * u2 <= - ulpTowardZero
                                        _    -> 2 * abs u2 <= abs ulpTowardZero) ()
                         (v1, v2) = if (-2) * u2 == ulpTowardZero then
                                      (u1 - ulpTowardZero, ulpTowardZero + u2)
                                    else
                                      (u1, u2)
                         !_ = assert (v1 + v2 == u1 + u2) ()
                         r1 = scaleFloat exy v1
                         -- !_ = assert (r1 == roundTiesTowardZero (fromRationalR (toRational x * toRational y))) ()
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
                                  -- !_ = assert (r2 == roundTiesTowardZero (fromRationalR (toRational x * toRational y - toRational r1))) ()
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
{-# SPECIALIZE augmentedMultiplication :: Float -> Float -> (Float, Float) #-}
{-# SPECIALIZE augmentedMultiplication :: Double -> Double -> (Double, Double) #-}
