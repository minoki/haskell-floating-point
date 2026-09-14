module RemainderSpec where
import           Data.Proxy
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

prop_remainder :: (RealFloat a, Show a) => Proxy a -> a -> a -> Property
prop_remainder _ x y
  | isFinite x && isFinite y && y /= 0 =
    let n = round (toRational x / toRational y)
        r = toRational x - toRational y * fromInteger n
        r' = if r == 0 then x * 0 else fromRational r
    in remainder x y `sameFloatP` r'
  | isFinite x && isInfinite y = remainder x y `sameFloatP` x
  | otherwise = isNaN (remainder x y) === True
{-# INLINABLE prop_remainder #-}

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "remainder" $ forAllFloats2 (prop_remainder proxy)
  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    prop "remainder" $ forAllFloats2 (prop_remainder proxy)
