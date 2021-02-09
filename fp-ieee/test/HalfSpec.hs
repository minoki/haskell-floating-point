{-# LANGUAGE CPP #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HalfSpec where
import           AugmentedArithSpec (augmentedAddition_viaRational,
                                     augmentedMultiplication_viaRational)
import qualified AugmentedArithSpec
import qualified ClassificationSpec
import           Control.Monad
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Int
import           Data.Proxy
import           Data.Ratio
import           FMASpec (fusedMultiplyAdd_generic,
                          fusedMultiplyAdd_viaRational)
import qualified FMASpec
import qualified NaNSpec
import qualified NextFloatSpec
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Numeric.Floating.IEEE.NaN (setPayloadSignaling)
import           Numeric.Half
import qualified RoundingSpec
import qualified RoundToIntegralSpec
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           TwoSumSpec (twoProduct_generic)
import qualified TwoSumSpec
import           Util

-- orphan instances
instance Arbitrary Half where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkDecimal

instance Random Half where
  -- Half:
  --   emin = -14, emax = 15
  --   precision = 11 bits
  --   maxFinite = 0xffe0 (65504)
  randomR (lo,hi) g = let (x,g') = random g
                      in (lo + x * (hi - lo), g') -- TODO: avoid overflow
  random g = let x :: Int32
                 (x,g') = random g
             in (fromRational (toInteger x % 2^(16 :: Int)), g')

isInfiniteWorkaround :: (Half -> Property) -> (Half -> Property)
isInfiniteIsKnownToBeBuggy :: Bool
#if MIN_VERSION_half(0,3,1)
isInfiniteWorkaround = id
isInfiniteIsKnownToBeBuggy = False
#else
-- Workaround for https://github.com/ekmett/half/issues/23
isInfiniteWorkaround f x = not (isNaN x) ==> f x
isInfiniteIsKnownToBeBuggy = True
#endif

spec :: Spec
spec = mapSpecItem_ (allowFailure "Half's fromRational may be incorrect") $ do
  let proxy :: Proxy Half
      proxy = Proxy
  prop "classify" $ forAllFloats $ isInfiniteWorkaround $ ClassificationSpec.prop_classify proxy
  prop "classify (generic)" $ forAllFloats $ isInfiniteWorkaround $ ClassificationSpec.prop_classify (Proxy :: Proxy (Identity Half)) . Identity
  prop "totalOrder" $ forAllFloats2 $ ClassificationSpec.prop_totalOrder proxy
  prop "totalOrder (generic)" $ forAllFloats2 (ClassificationSpec.prop_totalOrder (Proxy :: Proxy (Identity Half)) `on` Identity)
  prop "twoSum" $ forAllFloats2 $ TwoSumSpec.prop_twoSum proxy
  prop "twoProduct" $ forAllFloats2 $ TwoSumSpec.prop_twoProduct proxy twoProduct
  prop "twoProduct_generic" $ forAllFloats2 $ TwoSumSpec.prop_twoProduct proxy twoProduct_generic
  let casesForHalf :: [(Half, Half, Half, Half)]
      casesForHalf = [ (-0, 0, -0, -0)
                     , (-0, -0, -0, 0)
                       -- TODO: Add more
                     ]
  FMASpec.checkFMA "fusedMultiplyAdd (default)"      fusedMultiplyAdd             casesForHalf
  FMASpec.checkFMA "fusedMultiplyAdd (generic)"      fusedMultiplyAdd_generic     casesForHalf
  FMASpec.checkFMA "fusedMultiplyAdd (via Rational)" fusedMultiplyAdd_viaRational casesForHalf
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
  prop "scaleFloatR vs fromRationalR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_scaleFloatR_vs_fromRationalR proxy)
  prop "scaleFloatR vs encodeFloatR" $ RoundingSpec.eachStrategy (RoundingSpec.prop_scaleFloatR_vs_encodeFloatR proxy)
  prop "result of fromIntegerR" $ \x -> RoundingSpec.prop_order proxy (fromIntegerR x)
  prop "result of fromRationalR" $ \x -> RoundingSpec.prop_order proxy (fromRationalR x)
  prop "result of encodeFloatR" $ \m k -> RoundingSpec.prop_order proxy (encodeFloatR m k)
  prop "addToOdd" $ forAllFloats2 $ RoundingSpec.prop_addToOdd proxy

  prop "roundToIntegral" $ RoundToIntegralSpec.prop_roundToIntegral proxy
  RoundToIntegralSpec.checkCases proxy

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
  prop "classify" $ forAllFloats $ isInfiniteWorkaround $ NaNSpec.prop_classify proxy
  when (not isInfiniteIsKnownToBeBuggy) $ do
    prop "classify (signaling NaN)" $ NaNSpec.prop_classify proxy (setPayloadSignaling 123)
  prop "signaling NaN propagation" $ NaNSpec.prop_signalingNaN proxy
  prop "totalOrder" $ forAllFloats2 $ NaNSpec.prop_totalOrder proxy

  when isInfiniteIsKnownToBeBuggy $ do
    runIO $ putStrLn "Half's isInfinite is known to be buggy on this version. Some tests were skipped."
