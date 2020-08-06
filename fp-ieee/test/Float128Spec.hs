{-# LANGUAGE CPP #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Float128Spec where
import qualified AugmentedArithSpec
import qualified ClassificationSpec
import           Control.Monad
import           Data.Functor.Identity
import           Data.Int
import           Data.Proxy
import           Data.Ratio
import qualified FMASpec
import qualified NaNSpec
import qualified NextFloatSpec
import           Numeric.Float128
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Numeric.Floating.IEEE.NaN (setPayloadSignaling)
import qualified RoundingSpec
import qualified RoundToIntegralSpec
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import qualified TwoSumSpec
import           Util

-- orphan instances
instance Arbitrary Float128 where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

instance Random Float128 where
  -- Float128:
  --   emin = -14, emax = 15
  --   precision = 11 bits
  --   maxFinite = 0xffe0 (65504)
  randomR (lo,hi) g = let (x,g') = random g
                      in (lo + x * (hi - lo), g') -- TODO: avoid overflow
  random g = let x :: Int64
                 (x,g') = random g
             in (fromRational (toInteger x % 2^(16 :: Int)), g') -- TODO

spec :: Spec
spec = mapSpecItem_ (allowFailure "Float128's fromRational and round may be incorrect") $ do
  let proxy :: Proxy Float128
      proxy = Proxy
  prop "classify" $ forAllFloats $ ClassificationSpec.prop_classify proxy
  prop "classify (generic)" $ forAllFloats $ ClassificationSpec.prop_classify (Proxy :: Proxy (Identity Float128)) . Identity
  prop "twoSum" $ forAllFloats2 $ TwoSumSpec.prop_twoSum proxy
  prop "twoProduct" $ forAllFloats2 $ TwoSumSpec.prop_twoProduct proxy twoProduct
  prop "twoProduct_generic" $ forAllFloats2 $ TwoSumSpec.prop_twoProduct proxy twoProduct_generic
  let casesForFloat128 :: [(Float128, Float128, Float128, Float128)]
      casesForFloat128 = [ (-0, 0, -0, -0)
                     , (-0, -0, -0, 0)
                       -- TODO: Add more
                     ]
  FMASpec.checkFMA         "fusedMultiplyAdd (default)"             fusedMultiplyAdd             casesForFloat128
  FMASpec.checkFMA_generic "fusedMultiplyAdd (default, generic)"    fusedMultiplyAdd             casesForFloat128
  FMASpec.checkFMA         "fusedMultiplyAdd (twoProduct)"          fusedMultiplyAdd_twoProduct  casesForFloat128
  FMASpec.checkFMA_generic "fusedMultiplyAdd (twoProduct, generic)" fusedMultiplyAdd_twoProduct  casesForFloat128
  FMASpec.checkFMA         "fusedMultiplyAdd (via Rational)"        fusedMultiplyAdd_viaRational casesForFloat128
  prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ NextFloatSpec.prop_nextUp_nextDown proxy
  prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ NextFloatSpec.prop_nextDown_nextUp proxy
  prop "augmentedAddition/equality" $ forAllFloats2 $ \(x :: Float128) y ->
    isFinite x && isFinite y ==>
    let (s,t) = augmentedAddition x y
    in isFinite s ==> isFinite t .&&. toRational s + toRational t === toRational x + toRational y
  prop "augmentedAddition" $ forAllFloats2 $ \(x :: Float128) y ->
    augmentedAddition x y `sameFloatPairP` augmentedAddition_viaRational x y
  prop "augmentedMultiplication" $ forAllFloats2 $ \(x :: Float128) y ->
    augmentedMultiplication x y `sameFloatPairP` augmentedMultiplication_viaRational x y

  prop "fromIntegerR vs fromRationalR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_fromIntegerR_vs_fromRationalR proxy)
  prop "fromIntegerR vs encodeFloatR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_fromIntegerR_vs_encodeFloatR proxy)
  prop "fromRationalR vs encodeFloatR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_fromRationalR_vs_encodeFloatR proxy)
  prop "fromRationalR vs fromRational" $ RoundingSpec.prop_fromRationalR_vs_fromRational proxy
  prop "result of fromIntegerR" $ \x -> RoundingSpec.prop_order proxy (fromIntegerR x)
  prop "result of fromRationalR" $ \x -> RoundingSpec.prop_order proxy (fromRationalR x)
  prop "result of encodeFloatR" $ \m k -> RoundingSpec.prop_order proxy (encodeFloatR m k)
  prop "add_roundToOdd" $ forAllFloats2 $ RoundingSpec.prop_add_roundToOdd proxy

  prop "roundToIntegral" $ RoundToIntegralSpec.prop_roundToIntegral proxy
  RoundToIntegralSpec.checkCases proxy

{-
  prop "copySign" $ forAllFloats2 $ NaNSpec.prop_copySign proxy
  prop "isSignMinus" $ forAllFloats $ NaNSpec.prop_isSignMinus proxy
  prop "isSignaling" $ NaNSpec.prop_isSignaling proxy
  prop "setPayload/getPayload" $ NaNSpec.prop_setPayload_getPayload proxy
  prop "setPayload/0" $ NaNSpec.prop_setPayload proxy 0
  prop "setPayload/0x1p9" $ NaNSpec.prop_setPayload proxy 0x1p9
  prop "setPayload/Int" $ NaNSpec.prop_setPayload proxy . (fromIntegral :: Int -> Float128)
  prop "setPayloadSignaling/0" $ NaNSpec.prop_setPayloadSignaling proxy 0
  prop "setPayloadSignaling/0x1p9" $ NaNSpec.prop_setPayloadSignaling proxy 0x1p9
  prop "setPayloadSignaling/Int" $ NaNSpec.prop_setPayloadSignaling proxy . (fromIntegral :: Int -> Float128)
  prop "classify" $ forAllFloats $ NaNSpec.prop_classify proxy
  prop "classify (signaling NaN)" $ NaNSpec.prop_classify proxy (setPayloadSignaling 123)
  prop "signaling NaN propagation" $ NaNSpec.prop_signalingNaN proxy
-}
