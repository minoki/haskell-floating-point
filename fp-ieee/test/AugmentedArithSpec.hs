{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AugmentedArithSpec where
import           Control.Monad
import           Numeric
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

sameFloatPairP :: (RealFloat a, Show a) => (a, a) -> (a, a) -> Property
sameFloatPairP (x,y) (x',y') = counterexample (showPair x y . showString (interpret res) . showPair x' y' $ "") res
  where
    showPair s t = showChar '(' . showHFloat s . showChar ',' . showHFloat t . showChar ')'
    res = x `sameFloat` x' && y `sameFloat` y'
    interpret True  = " === "
    interpret False = " =/= "

testUnary :: (RealFloat b, Show a) => String -> (a -> b) -> [(a, b)] -> Spec
testUnary name f sp = forM_ sp $ \(a,result) -> do
  let label = showString name . showChar ' ' . showsPrec 11 a . showString " should be " . showHFloat result $ ""
  it label $ f a `sameFloatP` result

testAugmented :: (RealFloat a, Show a) => String -> (a -> a -> (a, a)) -> [(a, a, a, a)] -> Spec
testAugmented name f cases = forM_ cases $ \(a,b,r1,r2) -> do
  let label = showString name . showChar ' ' . showHFloat a . showChar ' ' . showHFloat b . showString " should be (" . showHFloat r1 . showChar ',' . showHFloat r2 $ ")"
  it label $ f a b `sameFloatPairP` (r1,r2)

spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "Double" $ do
    do -- roundTiesTowardZero
      let cases :: [(Rational, Double)]
          cases = [(0x1.ffff_ffff_ffff_f8p1023, maxFinite)
                  ,(0x1.ffff_ffff_ffff_f8p1023 + 1/723, 1/0)
                  ,(0x1.ffff_ffff_ffff_f8p1023 - 1/255, maxFinite)
                  ,(0xdead_beef.8p-1074, 0xdead_beefp-1074)
                  ,(0xdead_beef.9p-1074, 0xdead_bef0p-1074)
                  ,(-0xdead_beef.7p-1074, -0xdead_beefp-1074)
                  ,(-0x0.8p-1074, -0)
                  ,(-0x0.80007p-1074, -0x1p-1074)
                  ]
      testUnary "roundTiesTowardZero" (roundTiesTowardZero . fromRationalR) cases

    do -- augmentedAddition
      prop "augmentedAddition/equality" $ forAllFloats2 $ \(x :: Double) y ->
        isFinite x && isFinite y ==>
        let (s,t) = augmentedAddition x y
        in isFinite s ==> isFinite t .&&. toRational s + toRational t === toRational x + toRational y
      let cases :: [(Double, Double, Double, Double)]
          cases = [ (-0, -0, -0, -0)
                  ]
      testAugmented "augmentedAddition" augmentedAddition cases
      testAugmented "augmentedAddition_viaRational" augmentedAddition_viaRational cases
      prop "augmentedAddition" $ forAllFloats2 $ \(x :: Double) y ->
        augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y

    do -- augmentedMultiplication
      let cases :: [(Double, Double, Double, Double)]
          cases = [ (-0x1.3deed726aad4p-1023, 0x1.e179bde0a1dd2p-1, -0x1.2afa79f9d38c6p-1023, 0x0p+0)
                  , (-0x1.8eb0e02044f68p-1022, -0x1.c93b83a5751c8p-2, 0x1.640b37f1b9d02p-1023,-0x0p+0)
                  , (0x1.b877a1cd61478p-1023, -0x1.7a77bb9df06dap-1, -0x1.459753aa4d2bep-1023, -0x0p+0)
                  , (-0x1.d25f2402fe726p-1, -0x1.0b42f4e9eb842p-1, 0x1.e6e335433c1f9p-2, -0x1.bb70c80f1834p-58)
                  ]
      testAugmented "augmentedMultiplication" augmentedMultiplication cases
      testAugmented "augmentedMultiplication_viaRational" augmentedMultiplication_viaRational cases
      prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Double) y ->
        augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y

  describe "Float" $ do
    do -- roundTiesTowardZero
      let cases :: [(Rational, Float)]
          cases = [ (0x1.ffff_ffp127, maxFinite)
                  , (0x1.ffff_ffp127 + 1/723, 1/0)
                  , (0x1.ffff_ffp127 - 1/255, maxFinite)
                  , (0xbeef.8p-149, 0xbeefp-149)
                  , (0xbeef.9p-149, 0xbef0p-149)
                  , (-0xbeef.7p-149, -0xbeefp-149)
                  , (-0x0.8p-149, -0)
                  , (-0x0.80007p-149, -0x1p-149)
                  ]
      testUnary "roundTiesTowardZero" (roundTiesTowardZero . fromRationalR) cases

    do -- augmentedAddition
      prop "augmentedAddition/equality" $ forAllFloats2 $ \(x :: Float) y ->
        isFinite x && isFinite y ==>
        let (s,t) = augmentedAddition x y
        in isFinite s ==> isFinite t .&&. toRational s + toRational t === toRational x + toRational y
      let cases :: [(Float, Float, Float, Float)]
          cases = [(-0, -0, -0, -0)]
      testAugmented "augmentedAddition" augmentedAddition cases
      testAugmented "augmentedAddition_viaRational" augmentedAddition_viaRational cases
      prop "augmentedAddition" $ forAllFloats2 $ \(x :: Float) y ->
        augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y

    do -- augmentedMultiplication
      let cases :: [(Float, Float, Float, Float)]
          cases = [ (0x1.b8508p-130,  -0x1.93994p-4,  -0x1.5b17p-133,   -0x0p+0)
                  , (0x1.5433bcp-126, -0x1.69a04p-1,  -0x1.e091e8p-127, -0x0p+0)
                  , (0x1.c7363p-128,  -0x1.c5d164p-1, -0x1.937b98p-128, -0x0p+0)
                  , (-0x1.a31946p0,   -0x1p-127,       0x1.a31944p-127,  0x0p+0)
                  ]
      testAugmented "augmentedMultiplication" augmentedMultiplication cases
      testAugmented "augmentedMultiplication_viaRational" augmentedMultiplication_viaRational cases
      prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Float) y ->
        augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y

{-# NOINLINE spec #-}
