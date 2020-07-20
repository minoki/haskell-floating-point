module ClassificationSpec where
import           Data.Functor.Identity
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

prop_classify :: (RealFloat a, Show a) => a -> Property
prop_classify x = conjoin
  [ counterexample "NegativeInfinity" $ (c == NegativeInfinity) === (x < 0 && isInfinite x)
  , counterexample "NegativeNormal" $ (c == NegativeNormal) === (x < 0 && isNormal x)
  , counterexample "NegativeSubnormal" $ (c == NegativeSubnormal) === (x < 0 && isDenormalized x)
  , counterexample "NegativeZero" $ (c == NegativeZero) === (isNegativeZero x)
  , counterexample "PositiveZero" $ (c == PositiveZero) === (x == 0 && not (isNegativeZero x))
  , counterexample "PositiveSubnormal" $ (c == PositiveSubnormal) === (x > 0 && isDenormalized x)
  , counterexample "PositiveNormal" $ (c == PositiveNormal) === (x > 0 && isNormal x)
  , counterexample "PositiveInfinity" $ (c == PositiveInfinity) === (x > 0 && isInfinite x)
  , counterexample "isNaN" $ isNaN x === (c == SignalingNaN || c == QuietNaN)
  , counterexample "isInfinite" $ isInfinite x === (c == NegativeInfinity || c == PositiveInfinity)
  , counterexample "isNormal" $ isNormal x === (c == NegativeNormal || c == PositiveNormal)
  , counterexample "isDenormalized" $ isDenormalized x === (c == NegativeSubnormal || c == PositiveSubnormal)
  , counterexample "isZero" $ isZero x === (c == NegativeZero || c == PositiveZero)
  , counterexample "isFinite" $ isFinite x === (c `elem` [NegativeNormal, NegativeSubnormal, NegativeZero, PositiveZero, PositiveSubnormal, PositiveNormal])
  , counterexample "isSignMinus" $ isSignMinus x === (c `elem` [NegativeInfinity, NegativeNormal, NegativeSubnormal, NegativeZero]) -- isSignMinus doesn't handle negative NaNs
  ]
  where c = classify x
{-# SPECIALIZE prop_classify :: Float -> Property, Double -> Property #-}

spec :: Spec
spec = do
  describe "Double" $ do
    prop "classify" $ forAllFloats (prop_classify :: Double -> Property)
  describe "Double (generic)" $ do
    prop "classify" $ forAllFloats $ (prop_classify :: Identity Double -> Property) . Identity
  describe "Float" $ do
    prop "classify" $ forAllFloats (prop_classify :: Float -> Property)
  describe "Float (generic)" $ do
    prop "classify" $ forAllFloats $ (prop_classify :: Identity Float -> Property) . Identity
