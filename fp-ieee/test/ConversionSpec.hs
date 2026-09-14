module ConversionSpec where
import           Data.Proxy
import           Numeric
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Util

default ()

prop_conversion :: (RealFloat a, RealFloat b, Show a, Show b) => Proxy a -> Proxy b -> a -> Property
prop_conversion _ proxyB x =
  let y = realFloatToFrac x `asProxyTypeOf` proxyB
      y' | isInfinite x = if y > 0 then 1 / 0 else -(1 / 0)
         | isNaN x = 0 / 0
         | isNegativeZero x = -0
         | otherwise = fromRat (toRational x)
  in y `sameFloatP` y'
{-# INLINABLE prop_conversion #-}

{-# NOINLINE spec #-}
spec :: Spec
spec = modifyMaxSuccess (* 1000) $ do
  let proxyFloat :: Proxy Float
      proxyFloat = Proxy
      proxyDouble :: Proxy Double
      proxyDouble = Proxy
  prop "Float->Double" $ forAllFloats $ prop_conversion proxyFloat proxyDouble
  prop "Double->Float" $ forAllFloats $ prop_conversion proxyDouble proxyFloat
