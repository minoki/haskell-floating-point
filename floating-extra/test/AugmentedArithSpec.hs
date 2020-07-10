{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
module AugmentedArithSpec (spec) where
import Numeric.Floating.Extra.IEEE
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (classify)
import Util
import Numeric
import Control.Monad

augmentedAddition_viaRational :: forall a. (RealFloat a, Show a) => a -> a -> (a, a)
augmentedAddition_viaRational x y
  | isFinite x && isFinite y && (x /= 0 || y /= 0) =
    let z :: Rational
        z' :: a
        z = toRational x + toRational y
        z' = roundTiesTowardZero z
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w' :: a
             w = z - toRational z'
             w' = roundTiesTowardZero w
         in if w' == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x + y
                in (z, z)

augmentedMultiplication_viaRational :: forall a. (RealFloat a, Show a) => a -> a -> (a, a)
augmentedMultiplication_viaRational x y
  | isFinite x && isFinite y && (x /= 0 || y /= 0) =
    let z :: Rational
        z' :: a
        z = toRational x * toRational y
        z' = roundTiesTowardZero z
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w' :: a
             w = z - toRational z'
             w' = roundTiesTowardZero w
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x * y
                in (z, z)

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

spec :: Spec
spec = do
  describe "Double" $ do
    let rtzSpecialValues :: [(Rational, Double)]
        rtzSpecialValues = [(0x1.ffff_ffff_ffff_f8p1023, maxFinite)
                           ,(0x1.ffff_ffff_ffff_f8p1023 + 1/723, 1/0)
                           ,(0x1.ffff_ffff_ffff_f8p1023 - 1/255, maxFinite)
                           ,(0xdead_beef.8p-1074, 0xdead_beefp-1074)
                           ,(0xdead_beef.9p-1074, 0xdead_bef0p-1074)
                           ,(-0xdead_beef.7p-1074, -0xdead_beefp-1074)
                           ,(-0x0.8p-1074, -0)
                           ,(-0x0.80007p-1074, -0x1p-1074)
                           ]
    testUnary "roundTiesTowardZero" roundTiesTowardZero rtzSpecialValues
    prop "augmentedAddition" $ forAllFloats2 $ \(x :: Double) y ->
      augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y
    prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Double) y ->
      augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y
  describe "Float" $ do
    let rtzSpecialValues :: [(Rational, Float)]
        rtzSpecialValues = [(0x1.ffff_ffp127, maxFinite)
                           ,(0x1.ffff_ffp127 + 1/723, 1/0)
                           ,(0x1.ffff_ffp127 - 1/255, maxFinite)
                           ,(0xbeef.8p-149, 0xbeefp-149)
                           ,(0xbeef.9p-149, 0xbef0p-149)
                           ,(-0xbeef.7p-149, -0xbeefp-149)
                           ,(-0x0.8p-149, -0)
                           ,(-0x0.80007p-149, -0x1p-149)
                           ]
    testUnary "roundTiesTowardZero" roundTiesTowardZero rtzSpecialValues
    prop "augmentedAddition" $ forAllFloats2 $ \(x :: Float) y ->
      augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y
    prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Float) y ->
      augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y
{-# NOINLINE spec #-}
