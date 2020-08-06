{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AugmentedArithSpec where
import           Control.Monad
import           Numeric
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           RoundingSpec (RoundTiesTowardZero (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

augmentedAddition_viaRational :: (RealFloat a, Show a) => a -> a -> (a, a)
augmentedAddition_viaRational x y
  | isFinite x && isFinite y && (x /= 0 || y /= 0) =
    let z :: Rational
        z = toRational x + toRational y
        z' = roundTiesTowardZero (fromRationalR z) `asTypeOf` x
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w = z - toRational z'
             w' = roundTiesTowardZero (fromRationalR w) `asTypeOf` x
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x + y
                in (z, z)

augmentedMultiplication_viaRational :: (RealFloat a, Show a) => a -> a -> (a, a)
augmentedMultiplication_viaRational x y
  | isFinite x && isFinite y && x * y /= 0 =
    let z :: Rational
        z = toRational x * toRational y
        z' = roundTiesTowardZero (fromRationalR z) `asTypeOf` x
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w = z - toRational z'
             w' = roundTiesTowardZero (fromRationalR w) `asTypeOf` x
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x * y
                in (z, z)

testAugmented :: (RealFloat a, Show a) => (a -> a -> (a, a)) -> [(a, a, a, a)] -> Property
testAugmented f cases = conjoin
  [ let label = showHFloat a . showChar ' ' . showHFloat b $ ""
    in counterexample label $ f a b `sameFloatPairP` (r1,r2)
  | (a,b,r1,r2) <- cases
  ]

{-# NOINLINE spec #-}
spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "Double" $ do
    do -- augmentedAddition
      prop "augmentedAddition/equality" $ forAllFloats2 $ \(x :: Double) y ->
        isFinite x && isFinite y ==>
        let (s,t) = augmentedAddition x y
        in isFinite s ==> isFinite t .&&. toRational s + toRational t === toRational x + toRational y
      let cases :: [(Double, Double, Double, Double)]
          cases = [ (-0, -0, -0, -0)
                  ]
      prop "augmentedAddition" $ testAugmented augmentedAddition cases
      prop "augmentedAddition_viaRational" $ testAugmented augmentedAddition_viaRational cases
      prop "augmentedAddition" $ forAllFloats2 $ \(x :: Double) y ->
        augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y

    do -- augmentedMultiplication
      let cases :: [(Double, Double, Double, Double)]
          cases = [ (-0x1.3deed726aad4p-1023, 0x1.e179bde0a1dd2p-1, -0x1.2afa79f9d38c6p-1023, 0x0p+0)
                  , (-0x1.8eb0e02044f68p-1022, -0x1.c93b83a5751c8p-2, 0x1.640b37f1b9d02p-1023,-0x0p+0)
                  , (0x1.b877a1cd61478p-1023, -0x1.7a77bb9df06dap-1, -0x1.459753aa4d2bep-1023, -0x0p+0)
                  , (-0x1.d25f2402fe726p-1, -0x1.0b42f4e9eb842p-1, 0x1.e6e335433c1f9p-2, -0x1.bb70c80f1834p-58)
                  ]
      prop "augmentedMultiplication" $ testAugmented augmentedMultiplication cases
      prop "augmentedMultiplication_viaRational" $ testAugmented augmentedMultiplication_viaRational cases
      prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Double) y ->
        augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y

  describe "Float" $ do
    do -- augmentedAddition
      prop "augmentedAddition/equality" $ forAllFloats2 $ \(x :: Float) y ->
        isFinite x && isFinite y ==>
        let (s,t) = augmentedAddition x y
        in isFinite s ==> isFinite t .&&. toRational s + toRational t === toRational x + toRational y
      let cases :: [(Float, Float, Float, Float)]
          cases = [(-0, -0, -0, -0)]
      prop "augmentedAddition" $ testAugmented augmentedAddition cases
      prop "augmentedAddition_viaRational" $ testAugmented augmentedAddition_viaRational cases
      prop "augmentedAddition" $ forAllFloats2 $ \(x :: Float) y ->
        augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y

    do -- augmentedMultiplication
      let cases :: [(Float, Float, Float, Float)]
          cases = [ (0x1.b8508p-130,  -0x1.93994p-4,  -0x1.5b17p-133,   -0x0p+0)
                  , (0x1.5433bcp-126, -0x1.69a04p-1,  -0x1.e091e8p-127, -0x0p+0)
                  , (0x1.c7363p-128,  -0x1.c5d164p-1, -0x1.937b98p-128, -0x0p+0)
                  , (-0x1.a31946p0,   -0x1p-127,       0x1.a31944p-127,  0x0p+0)
                  ]
      prop "augmentedMultiplication" $ testAugmented augmentedMultiplication cases
      prop "augmentedMultiplication_viaRational" $ testAugmented augmentedMultiplication_viaRational cases
      prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Float) y ->
        augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y
