module ClassificationSpec (spec) where
import Numeric.Floating.Extra.IEEE
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (classify)
import Util

prop_classify :: (RealFloat a, Show a) => a -> Property
prop_classify x = let c = classify x
                  in (c == NegativeInfinity) === (x < 0 && isInfinite x)
                     .&&. (c == NegativeNormal) === (x < 0 && isNormal x)
                     .&&. (c == NegativeSubnormal) === (x < 0 && isDenormalized x)
                     .&&. (c == NegativeZero) === (isNegativeZero x)
                     .&&. (c == PositiveZero) === (x == 0 && not (isNegativeZero x))
                     .&&. (c == PositiveSubnormal) === (x > 0 && isDenormalized x)
                     .&&. (c == PositiveNormal) === (x > 0 && isNormal x)
                     .&&. (c == PositiveInfinity) === (x > 0 && isInfinite x)
                     .&&. isNaN x === (c == SignalingNaN || c == QuietNaN)
                     .&&. isInfinite x === (c == NegativeInfinity || c == PositiveInfinity)
                     .&&. isNormal x === (c == NegativeNormal || c == PositiveNormal)
                     .&&. isDenormalized x === (c == NegativeSubnormal || c == PositiveSubnormal)
                     .&&. isZero x === (c == NegativeZero || c == PositiveZero)
                     .&&. isFinite x === (c `elem` [NegativeNormal, NegativeSubnormal, NegativeZero, PositiveZero, PositiveSubnormal, PositiveNormal])
                     .&&. isSignMinus x === (c `elem` [NegativeInfinity, NegativeNormal, NegativeSubnormal, NegativeZero]) -- isSignMinus doesn't handle negative NaNs

spec :: Spec
spec = do
  describe "Double" $ do
    prop "classify" $ forAllFloats (prop_classify :: Double -> Property)
  describe "Float" $ do
    prop "classify" $ forAllFloats (prop_classify :: Float -> Property)
