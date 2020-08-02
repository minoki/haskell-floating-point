{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
module RoundingSpec where
import           Control.Monad
import           Data.Proxy
import           Data.Ratio
import           Numeric
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

newtype RoundToOdd a = RoundToOdd { roundToOdd :: a }
  deriving (Functor)

instance RoundingStrategy RoundToOdd where
  exact = RoundToOdd
  inexact _o _neg parity zero away | even parity = RoundToOdd away
                                   | otherwise = RoundToOdd zero
  doRound exact _o _neg parity zero away | not exact && even parity = RoundToOdd away
                                         | otherwise = RoundToOdd zero

newtype Exactness a = Exactness { isExact :: Bool }
  deriving (Functor)

instance RoundingStrategy Exactness where
  exact _ = Exactness True
  inexact _o _neg _parity _zero _away = Exactness False
  doRound exact _o _neg _parity _zero _away = Exactness exact

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
      up = roundTowardPositive result
      down = roundTowardNegative result
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

eachStrategy :: Testable prop => (forall f. RoundingStrategy f => (f a -> a) -> prop) -> Property
eachStrategy p = conjoin
  [ counterexample "roundTiesToEven" (p roundTiesToEven)
  , counterexample "roundTiesToAway" (p roundTiesToAway)
  , counterexample "roundTiesTowardZero" (p roundTiesTowardZero)
  , counterexample "roundTowardPositive" (p roundTowardPositive)
  , counterexample "roundTowardNegative" (p roundTowardNegative)
  , counterexample "roundTowardZero" (p roundTowardZero)
  , counterexample "roundToOdd" (p roundToOdd)
  ]

testUnary :: RealFloat b => (a -> b) -> [(String, a, b)] -> Property
testUnary f cases = conjoin
  [ counterexample t $ f a `sameFloatP` result
  | (t,a,result) <- cases
  ]

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "fromIntegerR vs fromRationalR" $ eachStrategy (prop_fromIntegerR_vs_fromRationalR proxy)
    prop "fromIntegerR vs encodeFloatR" $ eachStrategy (prop_fromIntegerR_vs_encodeFloatR proxy)
    prop "fromRationalR vs encodeFloatR" $ eachStrategy (prop_fromRationalR_vs_encodeFloatR proxy)
    prop "fromRationalR vs fromRational" $ prop_fromRationalR_vs_fromRational proxy
    prop "result of fromIntegerR" $ \x -> prop_order proxy (fromIntegerR x)
    prop "result of fromRationalR" $ \x -> prop_order proxy (fromRationalR x)
    prop "result of encodeFloatR" $ \m k -> prop_order proxy (encodeFloatR m k)
    prop "add_roundToOdd" $ forAllFloats2 $ prop_add_roundToOdd proxy
    let cases :: [(String, Rational, Double)]
        cases = [("0x1.ffff_ffff_ffff_f8p1023", 0x1.ffff_ffff_ffff_f8p1023, maxFinite)
                ,("(0x1.ffff_ffff_ffff_f8p1023 + 1/723)", 0x1.ffff_ffff_ffff_f8p1023 + 1/723, 1/0)
                ,("(0x1.ffff_ffff_ffff_f8p1023 - 1/255)", 0x1.ffff_ffff_ffff_f8p1023 - 1/255, maxFinite)
                ,("0xdead_beef.8p-1074", 0xdead_beef.8p-1074, 0xdead_beefp-1074)
                ,("0xdead_beef.9p-1074", 0xdead_beef.9p-1074, 0xdead_bef0p-1074)
                ,("-0xdead_beef.7p-1074", -0xdead_beef.7p-1074, -0xdead_beefp-1074)
                ,("-0x0.8p-1074", -0x0.8p-1074, -0)
                ,("-0x0.80007p-1074", -0x0.80007p-1074, -0x1p-1074)
                ]
    prop "roundTiesTowardZero" $ testUnary (roundTiesTowardZero . fromRationalR) cases

  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    prop "fromIntegerR vs fromRationalR" $ eachStrategy (prop_fromIntegerR_vs_fromRationalR proxy)
    prop "fromIntegerR vs encodeFloatR" $ eachStrategy (prop_fromIntegerR_vs_encodeFloatR proxy)
    prop "fromRationalR vs encodeFloatR" $ eachStrategy (prop_fromRationalR_vs_encodeFloatR proxy)
    prop "fromRationalR vs fromRational" $ prop_fromRationalR_vs_fromRational proxy
    prop "result of fromIntegerR" $ \x -> prop_order proxy (fromIntegerR x)
    prop "result of fromRationalR" $ \x -> prop_order proxy (fromRationalR x)
    prop "result of encodeFloatR" $ \m k -> prop_order proxy (encodeFloatR m k)
    prop "add_roundToOdd" $ forAllFloats2 $ prop_add_roundToOdd proxy

    let cases :: [(String, Rational, Float)]
        cases = [ ("0x1.ffff_ffp127", 0x1.ffff_ffp127, maxFinite)
                , ("(0x1.ffff_ffp127 + 1/723)", 0x1.ffff_ffp127 + 1/723, 1/0)
                , ("(0x1.ffff_ffp127 - 1/255)", 0x1.ffff_ffp127 - 1/255, maxFinite)
                , ("0xbeef.8p-149", 0xbeef.8p-149, 0xbeefp-149)
                , ("0xbeef.9p-149", 0xbeef.9p-149, 0xbef0p-149)
                , ("-0xbeef.7p-149", -0xbeef.7p-149, -0xbeefp-149)
                , ("-0x0.8p-149", -0x0.8p-149, -0)
                , ("-0x0.80007p-149", -0x0.80007p-149, -0x1p-149)
                ]
    prop "roundTiesTowardZero" $ testUnary (roundTiesTowardZero . fromRationalR) cases
