module NextFloatSpec where
import           Data.Proxy
import           Numeric.Decimal
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util (forAllFloats, sameFloatP)

isPositiveZero :: RealFloat a => a -> Bool
isPositiveZero x = x == 0 && not (isNegativeZero x)

prop_nextUp_nextDown :: (RealFloat a, Show a) => Proxy a -> a -> Property
prop_nextUp_nextDown _ x = x /= (-1/0) ==>
  let x' = nextUp (nextDown x)
  in x' `sameFloatP` x .||. (isPositiveZero x .&&. isNegativeZero x')

prop_nextDown_nextUp :: (RealFloat a, Show a) => Proxy a -> a -> Property
prop_nextDown_nextUp _ x = x /= (1/0) ==>
  let x' = nextDown (nextUp x)
  in x' `sameFloatP` x .||. (isNegativeZero x .&&. isPositiveZero x')

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Decimal32" $ do
    let proxy :: Proxy Decimal32
        proxy = Proxy
    prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ prop_nextUp_nextDown proxy
    prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ prop_nextDown_nextUp proxy

  describe "Decimal64" $ do
    let proxy :: Proxy Decimal64
        proxy = Proxy
    prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ prop_nextUp_nextDown proxy
    prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ prop_nextDown_nextUp proxy

  describe "Decimal128" $ do
    let proxy :: Proxy Decimal128
        proxy = Proxy
    prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ prop_nextUp_nextDown proxy
    prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ prop_nextDown_nextUp proxy
