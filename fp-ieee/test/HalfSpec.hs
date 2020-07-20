{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HalfSpec where
import qualified AugmentedArithSpec
import qualified ClassificationSpec
import           Data.Functor.Identity
import           Data.Int
import           Data.Proxy
import qualified FMASpec
import qualified NaNSpec
import qualified NextFloatSpec
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import qualified Numeric.Floating.IEEE.NaN (setPayloadSignaling)
import           Numeric.Half
import qualified RoundingSpec
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import qualified TwoSumSpec
import           Util

-- orphan instances
instance Arbitrary Half where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

instance Random Half where
  randomR (lo,hi) g = let (x,g') = random g
                      in (lo + x * (hi - lo), g') -- TODO: avoid overflow
  random g = let x :: Int64
                 (x,g') = random g
             in (fromIntegral x / 2^(32 :: Int), g') -- TODO: do better

spec :: Spec
spec = do
  let proxy :: Proxy Half
      proxy = Proxy
  prop "classify" $ forAllFloats $ ClassificationSpec.prop_classify proxy
  prop "classify (generic)" $ forAllFloats $ ClassificationSpec.prop_classify (Proxy :: Proxy (Identity Half)) . Identity
  prop "twoSum" $ forAllFloats2 $ TwoSumSpec.prop_twoSum proxy
  prop "twoProduct" $ forAllFloats2 $ TwoSumSpec.prop_twoProduct proxy twoProduct
  prop "twoProduct_generic" $ forAllFloats2 $ TwoSumSpec.prop_twoProduct proxy twoProduct_generic
  let casesForHalf :: [(Half, Half, Half, Half)]
      casesForHalf = [ (-0, 0, -0, -0)
                     , (-0, -0, -0, 0)
                       -- TODO: Add more
                     ]
  FMASpec.checkFMA         "fusedMultiplyAdd (default)"             fusedMultiplyAdd             casesForHalf
  FMASpec.checkFMA_generic "fusedMultiplyAdd (default, generic)"    fusedMultiplyAdd             casesForHalf
  FMASpec.checkFMA         "fusedMultiplyAdd (twoProduct)"          fusedMultiplyAdd_twoProduct  casesForHalf
  FMASpec.checkFMA_generic "fusedMultiplyAdd (twoProduct, generic)" fusedMultiplyAdd_twoProduct  casesForHalf
  FMASpec.checkFMA         "fusedMultiplyAdd (via Rational)"        fusedMultiplyAdd_viaRational casesForHalf
  prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ NextFloatSpec.prop_nextUp_nextDown proxy
  prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ NextFloatSpec.prop_nextDown_nextUp proxy
  prop "augmentedAddition/equality" $ forAllFloats2 $ \(x :: Half) y ->
    isFinite x && isFinite y ==>
    let (s,t) = augmentedAddition x y
    in isFinite s ==> isFinite t .&&. toRational s + toRational t === toRational x + toRational y
  prop "augmentedAddition" $ forAllFloats2 $ \(x :: Half) y ->
    augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y
  prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Half) y ->
    augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y

  prop "fromIntegerR vs fromRationalR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_fromIntegerR_vs_fromRationalR proxy)
  prop "fromIntegerR vs encodeFloatR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_fromIntegerR_vs_encodeFloatR proxy)
  prop "fromRationalR vs encodeFloatR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_fromRationalR_vs_encodeFloatR proxy)
  prop "fromRationalR vs fromRational" $ RoundingSpec.prop_fromRationalR_vs_fromRational proxy
  prop "result of fromIntegerR" $ \x -> RoundingSpec.prop_order proxy (fromIntegerR x)
  prop "result of fromRationalR" $ \x -> RoundingSpec.prop_order proxy (fromRationalR x)
  prop "result of encodeFloatR" $ \m k -> RoundingSpec.prop_order proxy (encodeFloatR m k)
  prop "add_roundToOdd" $ forAllFloats2 $ RoundingSpec.prop_add_roundToOdd proxy

  prop "copySign" $ forAllFloats2 $ NaNSpec.prop_copySign proxy
  prop "isSignMinus" $ forAllFloats $ NaNSpec.prop_isSignMinus proxy
  prop "isSignaling" $ NaNSpec.prop_isSignaling proxy
  prop "setPayload/getPayload" $ NaNSpec.prop_setPayload_getPayload proxy
  prop "setPayload/0" $ NaNSpec.prop_setPayload proxy 0
  prop "setPayload/0x1p9" $ NaNSpec.prop_setPayload proxy 0x1p9
  prop "setPayload/Int" $ NaNSpec.prop_setPayload proxy . (fromIntegral :: Int -> Half)
  prop "setPayloadSignaling/0" $ NaNSpec.prop_setPayloadSignaling proxy 0
  prop "setPayloadSignaling/0x1p9" $ NaNSpec.prop_setPayloadSignaling proxy 0x1p9
  prop "setPayloadSignaling/Int" $ NaNSpec.prop_setPayloadSignaling proxy . (fromIntegral :: Int -> Half)
  prop "classify" $ forAllFloats $ NaNSpec.prop_classify proxy
  prop "classify (signaling NaN)" $ NaNSpec.prop_classify proxy (setPayloadSignaling 123)
