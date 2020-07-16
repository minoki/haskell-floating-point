module TwoSumSpec (spec) where
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Util (forAllFloats2, sameFloatP)

prop_twoSum :: (RealFloat a, Show a) => a -> a -> Property
prop_twoSum x y = case twoSum x y of
  (s, t) -> x + y `sameFloatP` s .&&. (isFinite x && isFinite y && isFinite s ==> isFinite t .&&. toRational x + toRational y === toRational s + toRational t)

prop_twoProduct :: (RealFloat a, Show a) => a -> a -> Property
prop_twoProduct x y = case twoProduct x y of
  (s, t) -> x * y `sameFloatP` s .&&. (isFinite x && isFinite y && isFinite s ==> isFinite t .&&. fromRational (toRational x * toRational y - toRational s) === t) -- The result of twoProduct is not exact if the product underflows

spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "Double" $ do
    prop "twoSum" $ forAllFloats2 (prop_twoSum :: Double -> Double -> Property)
    prop "twoProduct" $ forAllFloats2 (prop_twoProduct :: Double -> Double -> Property)
  describe "Float" $ do
    prop "twoSum" $ forAllFloats2 (prop_twoSum :: Float -> Float -> Property)
    prop "twoProduct" $ forAllFloats2 (prop_twoProduct :: Float -> Float -> Property)
{-# NOINLINE spec #-}
