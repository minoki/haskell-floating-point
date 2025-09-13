{-# LANGUAGE HexFloatLiterals #-}
module NaNSpec where
import           Data.Proxy
import           Numeric.Floating.IEEE hiding (classify, compareByTotalOrder,
                                        isSignMinus)
import           Numeric.Floating.IEEE.NaN
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

default ()

prop_copySign :: (RealFloatNaN a) => Proxy a -> a -> a -> Property
prop_copySign _ x y = let x' = copySign x y
                      in isSignMinus x' === isSignMinus y

prop_isSignMinus :: (RealFloatNaN a) => Proxy a -> a -> Property
prop_isSignMinus _ x = isSignMinus (negate x) === not (isSignMinus x)

prop_isSignaling :: (RealFloatNaN a) => Proxy a -> Bool
prop_isSignaling proxy = let nan = (0 / 0) `asProxyTypeOf` proxy
                             -- common floating-point operations should generate a quiet NaN
                         in not (isSignaling nan)

prop_setPayload_getPayload :: (RealFloatNaN a) => Proxy a -> Property
prop_setPayload_getPayload proxy =
  let nan = (0 / 0) `asProxyTypeOf` proxy
      nan2 = setPayload (getPayload nan)
  in classify nan2 /= PositiveZero ==> compareByTotalOrder (abs nan) nan2 === EQ

prop_setPayload :: (RealFloatNaN a, Show a) => Proxy a -> a -> Property
prop_setPayload _ payload =
  let snan = setPayload payload
  in classify snan === PositiveZero .||. (not (isSignaling snan) .&&. classify snan === QuietNaN)

prop_setPayloadSignaling :: (RealFloatNaN a, Show a) => Proxy a -> a -> Property
prop_setPayloadSignaling _ payload =
  let snan = setPayloadSignaling payload
  in classify snan === PositiveZero .||. (isSignaling snan .&&. classify snan === SignalingNaN)

prop_classify :: (RealFloatNaN a, Show a) => Proxy a -> a -> Property
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
  , counterexample "isSignaling" $ isSignaling x === (c == SignalingNaN)
  , counterexample "isSignaling implies isNaN" $ if isSignaling x then isNaN x else True
  , counterexample "isInfinite" $ isInfinite x === (c == NegativeInfinity || c == PositiveInfinity)
  , counterexample "isNormal" $ isNormal x === (c == NegativeNormal || c == PositiveNormal)
  , counterexample "isDenormalized" $ isDenormalized x === (c == NegativeSubnormal || c == PositiveSubnormal)
  , counterexample "isZero" $ isZero x === (c == NegativeZero || c == PositiveZero)
  , counterexample "isFinite" $ isFinite x === (c `elem` [NegativeNormal, NegativeSubnormal, NegativeZero, PositiveZero, PositiveSubnormal, PositiveNormal])
  , counterexample "isSignMinus" $ if isSignMinus x then
                                     c `elem` [NegativeInfinity, NegativeNormal, NegativeSubnormal, NegativeZero, QuietNaN, SignalingNaN]
                                   else
                                     c `elem` [PositiveInfinity, PositiveNormal, PositiveSubnormal, PositiveZero, QuietNaN, SignalingNaN]
  -- , counterexample "class method" $ classify x === classifyDefault x
  ]
  where c = classify x
{-# SPECIALIZE prop_classify :: Proxy Float -> Float -> Property #-}
{-# SPECIALIZE prop_classify :: Proxy Double -> Double -> Property #-}

isQuietNaN :: (RealFloatNaN a) => a -> Bool
isQuietNaN x = isNaN x && not (isSignaling x)

prop_signalingNaN :: (RealFloatNaN a, Show a) => Proxy a -> Property
prop_signalingNaN proxy =
  let snan = setPayloadSignaling 123 `asProxyTypeOf` proxy -- Assume 123 is a valid payload
      qnan = setPayload 123 `asProxyTypeOf` proxy -- Assume 123 is a valid payload
  in conjoin
     [ counterexample "setPayloadSignaling produces a signaling NaN" $ isSignaling snan
     , counterexample "round'" $ isQuietNaN (round' snan)
     , counterexample "roundAway'" $ isQuietNaN (roundAway' snan)
     , counterexample "truncate'" $ isQuietNaN (truncate' snan)
     , counterexample "ceiling'" $ isQuietNaN (ceiling' snan)
     , counterexample "floor'" $ isQuietNaN (floor' snan)
     , counterexample "nextUp" $ isQuietNaN (nextUp snan)
     , counterexample "nextDown" $ isQuietNaN (nextDown snan)
     , counterexample "nextTowardZero" $ isQuietNaN (nextTowardZero snan)
     -- , counterexample "remainder" $ isQuietNaN (remainder snan snan)
     -- , counterexample "scaleFloat" $ isQuietNaN (scaleFloat 1 snan)
     , counterexample "+" $ isQuietNaN (snan + snan)
     , counterexample "-" $ isQuietNaN (snan - snan)
     , counterexample "*" $ isQuietNaN (snan * snan)
     , counterexample "/" $ isQuietNaN (snan / snan)
     , counterexample "sqrt" $ isQuietNaN (sqrt snan)
     , counterexample "fusedMultiplyAdd" $ isQuietNaN (fusedMultiplyAdd snan snan snan)
     , counterexample "fusedMultiplyAdd" $ isQuietNaN (fusedMultiplyAdd 0 0 snan)
     , counterexample "negate" $ isSignaling (negate snan)
     , counterexample "abs" $ isSignaling (abs snan)
     , counterexample "augmentedAddition" $ case augmentedAddition snan snan of (x, y) -> isQuietNaN x .&&. isQuietNaN y
     , counterexample "augmentedSubtraction" $ case augmentedSubtraction snan snan of (x, y) -> isQuietNaN x .&&. isQuietNaN y
     , counterexample "augmentedMultiplication" $ case augmentedMultiplication snan snan of (x, y) -> isQuietNaN x .&&. isQuietNaN y
     , counterexample "minimum" $ isQuietNaN (minimum' snan snan)
     , counterexample "minimumNumber" $ isQuietNaN (minimumNumber snan snan)
     , counterexample "maximum" $ isQuietNaN (maximum' snan snan)
     , counterexample "maximumNumber" $ isQuietNaN (maximumNumber snan snan)
     , counterexample "minimumMagnitude" $ isQuietNaN (minimumMagnitude snan snan)
     , counterexample "minimumMagnitudeNumber" $ isQuietNaN (minimumMagnitudeNumber snan snan)
     , counterexample "maximumMagnitude" $ isQuietNaN (maximumMagnitude snan snan)
     , counterexample "maximumMagnitudeNumber" $ isQuietNaN (maximumMagnitudeNumber snan snan)
     , counterexample "canonicalize" $ isQuietNaN (canonicalize snan)
     , counterexample "realFloatToFrac" $ isQuietNaN (realFloatToFrac snan `asProxyTypeOf` proxy)
     ]
{-# INLINE prop_signalingNaN #-}

prop_totalOrder :: RealFloatNaN a => Proxy a -> a -> a -> Property
prop_totalOrder proxy x y = let cmp_x_y = compareByTotalOrder x y
                                cmp_y_x = compareByTotalOrder y x
                                eq = equalByTotalOrder x y
                                -- cmp_reference = compareByTotalOrderDefault x y
                            in cmp_x_y === compare EQ cmp_y_x
                               .&&. (cmp_x_y == EQ) === eq
                               -- .&&. cmp_x_y === cmp_reference
                               .&&. (if x < y then cmp_x_y === LT else property True)
                               .&&. (if y < x then cmp_x_y === GT else property True)
                               .&&. equalByTotalOrder x x
                               .&&. equalByTotalOrder y y

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    let snan = setPayloadSignaling 123 `asProxyTypeOf` proxy -- Assume 123 is a valid payload
    prop "copySign" $ forAllFloats2 $ prop_copySign proxy
    prop "isSignMinus" $ forAllFloats $ prop_isSignMinus proxy
    prop "isSignaling" $ prop_isSignaling proxy
    prop "setPayload/getPayload" $ prop_setPayload_getPayload proxy
    prop "setPayload/0" $ prop_setPayload proxy 0
    prop "setPayload/0x1p24" $ prop_setPayload proxy 0x1p24
    prop "setPayload/Int" $ prop_setPayload proxy . (fromIntegral :: Int -> Float)
    prop "setPayloadSignaling/0" $ prop_setPayloadSignaling proxy 0
    prop "setPayloadSignaling/0x1p24" $ prop_setPayloadSignaling proxy 0x1p24
    prop "setPayloadSignaling/Int" $ prop_setPayloadSignaling proxy . (fromIntegral :: Int -> Float)
    prop "classify" $ forAllFloats $ prop_classify proxy
    prop "classify (signaling NaN)" $ prop_classify proxy (setPayloadSignaling 123)
    prop "signaling NaN propagation" $ prop_signalingNaN proxy
    prop "totalOrder" $ forAllFloats2 $ prop_totalOrder proxy
    prop "canonicalize" $ isQuietNaN (canonicalize snan)
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    let snan = setPayloadSignaling 123 `asProxyTypeOf` proxy -- Assume 123 is a valid payload
    prop "copySign" $ forAllFloats2 $ prop_copySign proxy
    prop "isSignMinus" $ forAllFloats $ prop_isSignMinus proxy
    prop "isSignaling" $ prop_isSignaling proxy
    prop "setPayload/getPayload" $ prop_setPayload_getPayload proxy
    prop "setPayload/0" $ prop_setPayload proxy 0
    prop "setPayload/0x1p53" $ prop_setPayload proxy 0x1p53
    prop "setPayload/Int" $ prop_setPayload proxy . (fromIntegral :: Int -> Double)
    prop "setPayloadSignaling/0" $ prop_setPayloadSignaling proxy 0
    prop "setPayloadSignaling/0x1p53" $ prop_setPayloadSignaling proxy 0x1p53
    prop "setPayloadSignaling/Int" $ prop_setPayloadSignaling proxy . (fromIntegral :: Int -> Double)
    prop "classify" $ forAllFloats $ prop_classify proxy
    prop "classify (signaling NaN)" $ prop_classify proxy (setPayloadSignaling 123)
    prop "signaling NaN propagation" $ prop_signalingNaN proxy
    prop "totalOrder" $ forAllFloats2 $ prop_totalOrder proxy
    prop "canonicalize" $ isQuietNaN (canonicalize snan)
