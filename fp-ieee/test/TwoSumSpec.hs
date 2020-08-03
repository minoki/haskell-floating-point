module TwoSumSpec where
import           Data.Proxy
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Util (forAllFloats2, sameFloatP)

prop_twoSum :: (RealFloat a, Show a) => Proxy a -> a -> a -> Property
prop_twoSum _ x y = exponent x < expMax && exponent y < expMax ==> case twoSum x y of
  (s, t) -> x + y `sameFloatP` s .&&. (isFinite x && isFinite y && isFinite s ==> isFinite t .&&. toRational x + toRational y === toRational s + toRational t)
  where (_,expMax) = floatRange x

prop_twoProduct :: (RealFloat a, Show a) => Proxy a -> (a -> a -> (a, a)) -> a -> a -> Property
prop_twoProduct _ tp x y = case tp x y of
  (s, t) -> x * y `sameFloatP` s .&&. (isFinite x && isFinite y && isFinite s ==> isFinite t .&&. fromRational (toRational x * toRational y - toRational s) === t) -- The result of twoProduct is not exact if the product underflows

{-# NOINLINE spec #-}
spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "twoSum" $ forAllFloats2 $ prop_twoSum proxy
    prop "twoProduct" $ forAllFloats2 $ prop_twoProduct proxy twoProduct
    prop "twoProduct_generic" $ forAllFloats2 $ prop_twoProduct proxy twoProduct_generic
  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    prop "twoSum" $ forAllFloats2 $ prop_twoSum proxy
    prop "twoProduct" $ forAllFloats2 $ prop_twoProduct proxy twoProduct
    prop "twoProduct_generic" $ forAllFloats2 $ prop_twoProduct proxy twoProduct_generic
    prop "twoProductFloat_viaDouble" $ forAllFloats2 $ prop_twoProduct proxy twoProductFloat_viaDouble
