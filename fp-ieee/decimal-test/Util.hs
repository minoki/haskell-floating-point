{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Util where
import           Control.Applicative
import           Data.Ratio
import           Numeric
import           Numeric.Decimal
import           Numeric.Floating.IEEE
import           System.Random
import           Test.QuickCheck

-- | Compares two floating point values.
--
-- Unlike @(==)@, @+0@ and @-0@ are considered distinct and NaNs are equal.
--
-- >>> sameFloat 0 (-0 :: Double)
-- False
-- >>> sameFloat (0/0) (0/0 :: Double)
-- True
sameFloat :: RealFloat a => a -> a -> Bool
sameFloat x y | isNaN x && isNaN y = True
              | x == 0 && y == 0 = isNegativeZero x == isNegativeZero y
              | otherwise = x == y

sameFloatP :: (RealFloat a, Show a) => a -> a -> Property
sameFloatP x y = counterexample (shows x . showString (interpret res) . showHFloat y $ "") res
  where
    res = sameFloat x y
    interpret True  = " === "
    interpret False = " =/= "

infix 4 `sameFloat`, `sameFloatP`

variousFloats :: forall a. (RealFloat a, Arbitrary a, Random a, Show a) => Gen a
variousFloats = frequency
  [ (10, arbitrary)
  , (10, choose (-1, 1))
  , (10, (* encodeFloat 1 expMin) <$> choose (-1, 1) ) -- subnormal or very small normal
  , (10, (* encodeFloat 1 (expMax-1)) <$> choose (-2, 2) ) -- infinity or very large normal
  , (1, pure 0) -- positive zero
  , (1, pure (-0)) -- negative zero
  , (1, pure (1/0)) -- positive infinity
  , (1, pure (-1/0)) -- negative infinity
  , (1, pure (0/0)) -- NaN
  , (1, pure maxFinite) -- max finite
  , (1, pure (-maxFinite)) -- min negative
  , (1, pure minPositive) -- min positive
  , (1, pure (-minPositive)) -- max negative
  ]
  where (expMin,expMax) = floatRange (undefined :: a)

forAllFloats :: (RealFloat a, Arbitrary a, Random a, Show a, Testable prop) => (a -> prop) -> Property
forAllFloats = forAllShow variousFloats (\x -> show x)

forAllFloats2 :: (RealFloat a, Arbitrary a, Random a, Show a, Testable prop) => (a -> a -> prop) -> Property
forAllFloats2 f = forAllFloats $ \x -> forAllFloats $ \y -> f x y

forAllFloats3 :: (RealFloat a, Arbitrary a, Random a, Show a, Testable prop) => (a -> a -> a -> prop) -> Property
forAllFloats3 f = forAllFloats $ \x -> forAllFloats $ \y -> forAllFloats $ \z -> f x y z

-- orphan instances
instance (FinitePrecision p, Rounding r) => Arbitrary (Decimal p r) where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

instance (FinitePrecision p, Rounding r) => Random (Decimal p r) where
  randomR (lo,hi) g = let (x,g') = random g
                      in (lo + x * (hi - lo), g') -- TODO: avoid overflow
  random g = let x :: Int
                 (x,g') = random g
             in (fromRational (toInteger x % 1000), g') -- TODO
