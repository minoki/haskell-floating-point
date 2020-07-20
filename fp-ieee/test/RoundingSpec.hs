{-# LANGUAGE RankNTypes #-}
module RoundingSpec where
import           Control.Monad
import           Data.Proxy
import           Data.Ratio
import           Numeric
import           Numeric.Floating.IEEE hiding (roundTiesToAway,
                                        roundTiesTowardZero)
import           Numeric.Floating.IEEE.Internal hiding (roundTiesToAway,
                                                 roundTiesTowardZero)
import           Numeric.Floating.IEEE.Internal.Rounding
import qualified Numeric.Floating.IEEE.Internal as Augmented (roundTiesTowardZero)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

prop_fromIntegerR_vs_fromRationalR :: (RealFloat a, RoundingStrategy f) => Proxy a -> (f a -> a) -> Integer -> Property
prop_fromIntegerR_vs_fromRationalR _ f m =
  let x = f (fromIntegerR m)
      y = f (fromRationalR (m % 1))
  in x `sameFloatP` y

prop_fromIntegerR_vs_encodeFloatR :: (RealFloat a, RoundingStrategy f) => Proxy a -> (f a -> a) -> Integer -> NonNegative Int -> Property
prop_fromIntegerR_vs_encodeFloatR _ f m (NonNegative k) =
  let x = f (fromIntegerR m)
      y = f (encodeFloatR (m * floatRadix x ^ k) (-k))
  in x `sameFloatP` y

prop_fromRationalR_vs_encodeFloatR :: (RealFloat a, RoundingStrategy f) => Proxy a -> (f a -> a) -> Integer -> Int -> Property
prop_fromRationalR_vs_encodeFloatR _ f m k =
  let x = f (fromRationalR (fromInteger m * fromInteger (floatRadix x) ^^ k))
      y = f (encodeFloatR m k)
  in x `sameFloatP` y

prop_fromRationalR_vs_fromRational :: RealFloat a => Proxy a -> Rational -> Property
prop_fromRationalR_vs_fromRational proxy q =
  let x = roundTiesToEven (fromRationalR q) `asProxyTypeOf` proxy
      y = fromRational q `asProxyTypeOf` proxy
  in x `sameFloatP` y

prop_order :: RealFloat a => Proxy a -> (forall f. RoundingStrategy f => f a) -> Property
prop_order _ result =
  let tiesToEven = roundTiesToEven result
      tiesToAway = roundTiesToAway result
      tiesTowardZero = roundTiesTowardZero result
      up = roundUpward result
      down = roundDownward result
      zero = roundTowardZero result
      toOdd = roundToOdd result
  in if isExact result then
       counterexample "exact case" $ conjoin
       [ counterexample "tiesToAway == tiesToEven" $ tiesToAway `sameFloatP` tiesToEven
       , counterexample "tiesTowardZero == tiesToEven" $ tiesTowardZero `sameFloatP` tiesToEven
       , counterexample "upward == tiesToEven" $ up `sameFloatP` tiesToEven
       , counterexample "downward == tiesToEven" $ down `sameFloatP` tiesToEven
       , counterexample "towardZero == tiesToEven" $ zero `sameFloatP` tiesToEven
       , counterexample "toOdd == tiesToEven" $ toOdd `sameFloatP` tiesToEven
       ]
     else
       counterexample "inexact case" $ conjoin
       [ counterexample "down < up" $ down < up
       , counterexample "down <= tiesToEven" $ down <= tiesToEven
       , counterexample "down <= tiesToAway" $ down <= tiesToAway
       , counterexample "down <= tiesTowardZero" $ down <= tiesTowardZero
       , counterexample "down <= towardZero" $ down <= zero
       , counterexample "down <= odd" $ down <= toOdd
       , counterexample "tiesToEven <= up" $ tiesToEven <= up
       , counterexample "tiesToAway <= up" $ tiesToAway <= up
       , counterexample "tiesTowardZero <= up" $ tiesTowardZero <= up
       , counterexample "towardZero <= up" $ zero <= up
       , counterexample "odd <= up" $ toOdd <= up
       , counterexample "nextUp down == up" $ nextUp down `sameFloatP` up
       , counterexample "down == nextDown up" $ down `sameFloatP` nextDown up
       , counterexample "abs towardZero < max (abs down) (abs up)" $ abs zero < max (abs down) (abs up)
       , counterexample "not (isMantissaEven toOdd)" $ not (isMantissaEven toOdd)
       ]

prop_add_roundToOdd :: RealFloat a => Proxy a -> a -> a -> Property
prop_add_roundToOdd _ x y = isFinite x && isFinite y && isFinite (x + y) ==>
  let z = add_roundToOdd x y
      w = if x == 0 && y == 0 then
            x + y
          else
            roundToOdd (fromRationalR (toRational x + toRational y))
  in z `sameFloatP` w

prop_roundTiesTowardZero :: RealFloat a => Proxy a -> Rational -> Property
prop_roundTiesTowardZero proxy x =
  let z = Augmented.roundTiesTowardZero x
      w = roundTiesTowardZero (fromRationalR x)
  in z `sameFloatP` w

eachStrategy :: Testable prop => (forall f. RoundingStrategy f => (f a -> a) -> prop) -> Property
eachStrategy p = conjoin
  [ counterexample "roundTiesToEven" (p roundTiesToEven)
  , counterexample "roundTiesToAway" (p roundTiesToAway)
  , counterexample "roundTiesTowardZero" (p roundTiesTowardZero)
  , counterexample "roundUpward" (p roundUpward)
  , counterexample "roundDownward" (p roundDownward)
  , counterexample "roundTowardZero" (p roundTowardZero)
  , counterexample "roundToOdd" (p roundToOdd)
  ]

spec :: Spec
spec = do
  describe "Double" $ do
    let proxyDouble :: Proxy Double
        proxyDouble = Proxy
    prop "fromIntegerR vs fromRationalR" $ eachStrategy (prop_fromIntegerR_vs_fromRationalR proxyDouble)
    prop "fromIntegerR vs encodeFloatR" $ eachStrategy (prop_fromIntegerR_vs_encodeFloatR proxyDouble)
    prop "fromRationalR vs encodeFloatR" $ eachStrategy (prop_fromRationalR_vs_encodeFloatR proxyDouble)
    prop "fromRationalR vs fromRational" $ prop_fromRationalR_vs_fromRational proxyDouble
    prop "result of fromIntegerR" $ \x -> prop_order proxyDouble (fromIntegerR x)
    prop "result of fromRationalR" $ \x -> prop_order proxyDouble (fromRationalR x)
    prop "result of encodeFloatR" $ \m k -> prop_order proxyDouble (encodeFloatR m k)
    prop "add_roundToOdd" $ forAllFloats2 $ prop_add_roundToOdd proxyDouble
    prop "roundTiesTowardZero" $ prop_roundTiesTowardZero proxyDouble
  describe "Float" $ do
    let proxyFloat :: Proxy Float
        proxyFloat = Proxy
    prop "fromIntegerR vs fromRationalR" $ eachStrategy (prop_fromIntegerR_vs_fromRationalR proxyFloat)
    prop "fromIntegerR vs encodeFloatR" $ eachStrategy (prop_fromIntegerR_vs_encodeFloatR proxyFloat)
    prop "fromRationalR vs encodeFloatR" $ eachStrategy (prop_fromRationalR_vs_encodeFloatR proxyFloat)
    prop "fromRationalR vs fromRational" $ prop_fromRationalR_vs_fromRational proxyFloat
    prop "result of fromIntegerR" $ \x -> prop_order proxyFloat (fromIntegerR x)
    prop "result of fromRationalR" $ \x -> prop_order proxyFloat (fromRationalR x)
    prop "result of encodeFloatR" $ \m k -> prop_order proxyFloat (encodeFloatR m k)
    prop "add_roundToOdd" $ forAllFloats2 $ prop_add_roundToOdd proxyFloat
    prop "roundTiesTowardZero" $ prop_roundTiesTowardZero proxyFloat
{-# NOINLINE spec #-}
