{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}
module Util
  ( module Util
  , mapSpecItem_ -- from Test.Hspec.Core.Spec
  ) where
import           Control.Applicative
import           Data.Ratio
import           Numeric
import           Numeric.Floating.IEEE
import           System.Random
import           Test.Hspec.Core.Spec
import           Test.QuickCheck

newtype ShowHexFloat a = ShowHexFloat a deriving (Eq,Ord,Arbitrary)

instance RealFloat a => Show (ShowHexFloat a) where
  showsPrec _prec (ShowHexFloat x) = showHFloat x

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

sameFloatP :: (RealFloat a) => a -> a -> Property
sameFloatP x y = counterexample (showHFloat x . showString (interpret res) . showHFloat y $ "") res
  where
    res = sameFloat x y
    interpret True  = " === "
    interpret False = " =/= "

sameFloatPairP :: (RealFloat a, Show a) => (a, a) -> (a, a) -> Property
sameFloatPairP (x,y) (x',y') = counterexample (showPair x y . showString (interpret res) . showPair x' y' $ "") res
  where
    showPair s t = showChar '(' . showHFloat s . showChar ',' . showHFloat t . showChar ')'
    res = x `sameFloat` x' && y `sameFloat` y'
    interpret True  = " === "
    interpret False = " =/= "

infix 4 `sameFloat`, `sameFloatP`, `sameFloatPairP`

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

variousIntegers :: Gen Integer
variousIntegers = frequency
  [ (10, arbitrary)
  , (10, elements [id,negate] <*> choose (2^52, 2^54))
  , (10, elements [id,negate] <*> choose (2^62, 2^64))
  , (10, elements [id,negate] <*> choose (2^100, 2^101))
  , (10, elements [id,negate] <*> choose (2^1020, 2^1030))
  , (5, elements [id,negate] <*> choose (2^1070, 2^1075))
  , (5, elements [id,negate] <*> choose (2^16382, 2^16384))
  , (3, elements [id,negate] <*> choose (2^16440, 2^16445))
  ]

variousRationals :: Gen Rational
variousRationals = liftA2 (%) variousIntegers (variousIntegers `suchThat` (/= 0))

forAllFloats :: (RealFloat a, Arbitrary a, Random a, Show a, Testable prop) => (a -> prop) -> Property
forAllFloats = forAllShow variousFloats (\x -> showHFloat x "")

forAllFloats2 :: (RealFloat a, Arbitrary a, Random a, Show a, Testable prop) => (a -> a -> prop) -> Property
forAllFloats2 f = forAllFloats $ \x -> forAllFloats $ \y -> f x y

forAllFloats3 :: (RealFloat a, Arbitrary a, Random a, Show a, Testable prop) => (a -> a -> a -> prop) -> Property
forAllFloats3 f = forAllFloats $ \x -> forAllFloats $ \y -> forAllFloats $ \z -> f x y z

allowFailure :: String -> Item a -> Item a
allowFailure message item@(Item { itemExample = origExample }) = item { itemExample = newExample }
  where
    newExample params around callback = do
      result <- origExample params around callback
      case result of
        Result { resultStatus = Test.Hspec.Core.Spec.Failure loc reason } -> do
          let message' = case reason of
                           NoReason -> message
                           _        -> message ++ ": " ++ show reason
          return result { resultStatus = Pending loc (Just message') }
        _ -> return result
