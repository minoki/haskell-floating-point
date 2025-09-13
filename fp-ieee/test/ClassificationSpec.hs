module ClassificationSpec where
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Proxy
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

default ()

prop_classify :: (RealFloat a, Show a) => Proxy a -> a -> Property
prop_classify _ x = conjoin
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
{-# SPECIALIZE prop_classify :: Proxy Float -> Float -> Property #-}
{-# SPECIALIZE prop_classify :: Proxy Double -> Double -> Property #-}

prop_totalOrder :: RealFloat a => Proxy a -> a -> a -> Property
prop_totalOrder proxy x y = let cmp_x_y = compareByTotalOrder x y
                                cmp_y_x = compareByTotalOrder y x
                            in cmp_x_y === compare EQ cmp_y_x
                               .&&. (if x < y then cmp_x_y === LT else property True)
                               .&&. (if y < x then cmp_x_y === GT else property True)
{-# SPECIALIZE prop_totalOrder :: Proxy Float -> Float -> Float -> Property #-}
{-# SPECIALIZE prop_totalOrder :: Proxy Double -> Double -> Double -> Property #-}

spec :: Spec
spec = do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "classify" $ forAllFloats $ prop_classify proxy
    prop "totalOrder" $ forAllFloats2 $ prop_totalOrder proxy
  describe "Double (generic)" $ do
    let proxy :: Proxy (Identity Double)
        proxy = Proxy
    prop "classify" $ forAllFloats $ prop_classify proxy . Identity
    prop "totalOrder" $ forAllFloats2 (prop_totalOrder proxy `on` Identity)
  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    prop "classify" $ forAllFloats $ prop_classify proxy
    prop "totalOrder" $ forAllFloats2 $ prop_totalOrder proxy
  describe "Float (generic)" $ do
    let proxy :: Proxy (Identity Float)
        proxy = Proxy
    prop "classify" $ forAllFloats $ prop_classify proxy . Identity
    prop "totalOrder" $ forAllFloats2 (prop_totalOrder proxy `on` Identity)
