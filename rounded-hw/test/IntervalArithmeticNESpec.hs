{-# LANGUAGE ScopedTypeVariables #-}
module IntervalArithmeticNESpec where
import           Data.Proxy
import           Numeric.Rounded.Hardware.Internal
import           Numeric.Rounded.Hardware.Interval.NonEmpty
import           Numeric.Rounded.Hardware.Interval.Class (makeInterval, equalAsSet, subset)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

data OrdPair a = OrdPair a a deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (OrdPair a) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 return $ OrdPair (min x y) (max x y)

verifyImplementation :: forall a. (Arbitrary a, Ord a, Show a, RoundedFractional a, RoundedSqrt a, RealFloatConstants a, RealFloat a) => Proxy a -> Spec
verifyImplementation _ = do
  prop "intervalAdd" $ \(OrdPair (x :: a) y) (OrdPair x' y') ->
    let iv1, iv2 :: Interval a
        iv1 = makeInterval (Rounded x) (Rounded y) + makeInterval (Rounded x') (Rounded y')
        iv2 = makeInterval (Rounded $ roundedAdd TowardNegInf x x') (Rounded $ roundedAdd TowardInf y y')
    in iv1 `equalAsSet` iv2
  prop "intervalSub" $ \(OrdPair (x :: a) y) (OrdPair x' y') ->
    let iv1, iv2 :: Interval a
        iv1 = makeInterval (Rounded x) (Rounded y) - makeInterval (Rounded x') (Rounded y')
        iv2 = makeInterval (Rounded $ roundedSub TowardNegInf x y') (Rounded $ roundedSub TowardInf y x')
    in iv1 `equalAsSet` iv2
  prop "intervalSqrt" $ \(OrdPair (NonNegative (x :: a)) (NonNegative y)) ->
    let iv1, iv2 :: Interval a
        iv1 = sqrt (makeInterval (Rounded x) (Rounded y))
        iv2 = makeInterval (Rounded $ roundedSqrt TowardNegInf x) (Rounded $ roundedSqrt TowardInf y)
    in iv1 `equalAsSet` iv2
  prop "powInt" $ \(OrdPair (x :: a) y) (NonNegative n) ->
    let iv :: Interval a
        iv = makeInterval (Rounded x) (Rounded y)
        iv_n = powInt iv n
    in fromRational ((toRational x)^n) `subset` iv_n .&&. fromRational ((toRational y)^n) `subset` iv_n

spec :: Spec
spec = do
  describe "Double" $ verifyImplementation (Proxy :: Proxy Double)
  describe "Float" $ verifyImplementation (Proxy :: Proxy Float)
