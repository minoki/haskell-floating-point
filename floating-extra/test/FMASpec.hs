{-# LANGUAGE CPP #-}
{-# LANGUAGE HexFloatLiterals #-}
module FMASpec (spec) where
import           Control.Monad
import           Data.Coerce
import           Data.Functor.Identity
import           Numeric
import           Numeric.Floating.IEEE
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Util (forAllFloats3, sameFloatP)

#if defined(USE_FFI)

foreign import ccall unsafe "fma"
  c_fma_double :: Double -> Double -> Double -> Double
foreign import ccall unsafe "fmaf"
  c_fma_float :: Float -> Float -> Float -> Float

#endif

casesForDouble :: [(Double, Double, Double, Double)]
casesForDouble =
  [ (0x1.af7da9fc47b3ep-1,     0x1p-1074,            -0x1p-1074, -0)
  , (0x1p512,                  0x1p512,              -0x1p1023,   0x1p1023)
  , (0x1.0000000000008p500,    0x1.1p500,             0x1p-1074,  0x1.1000000000009p1000)
  , (0x1.0000000000001p500,    0x1.8p500,            -0x1p-1074,  0x1.8000000000001p1000)
  , (0x1.ffffffc000000p512,    0x1.0000002p511,      -0x1p-1074,  0x1.fffffffffffffp1023) -- 0x1.ffffffc000000p512 * 0x1.0000002p511 == 0x1.fffffffffffff8p1023 (in Rational)
  , (-0x1.032ede48bbb28p-1022, 0x1.3cbc999ae14a8p-1, -0x1p-1074, -0x1.40accc50d63d2p-1023)
  ]

casesForFloat :: [(Float, Float, Float, Float)]
casesForFloat =
  [ (16777215, 268435520, 63.5, 0x1.000002p52)
  , (0x1.84ae30p125, 0x1.6p-141,    0x1p-149,       0x1.0b37c2p-15)
  , (0x1.000010p50,  0x1.1p50,      0x1p-149,       0x1.100012p100)
  , (0x1.000002p50,  0x1.8p50,     -0x1p-149,       0x1.800002p100)
  , (0x1.83bd78p4,  -0x1.cp118,    -0x1.344108p-2, -0x1.5345cap123)
  , (0x1p-149,       0x1.88dd0cp-1, 0x1.081ffp-127, 0x1.081ff4p-127)
  ]

testSpecialValues :: (RealFloat a, Show a) => String -> (a -> a -> a -> a) -> [(a, a, a, a)] -> Spec
testSpecialValues name f cases = forM_ cases $ \(a,b,c,result) -> do
  let label = showString name . showChar ' ' . showHFloat a . showChar ' ' . showHFloat b . showChar ' ' . showHFloat c . showString " should be " . showHFloat result $ ""
  it label $ f a b c `sameFloatP` result

checkFMA :: (RealFloat a, Show a, Arbitrary a, Random a) => String -> (a -> a -> a -> a) -> [(a, a, a, a)] -> Spec
checkFMA name f cases = do
  prop name $ forAllFloats3 $ \a b c -> do
    f a b c `sameFloatP` fusedMultiplyAdd_viaRational a b c
  testSpecialValues name f cases

-- Test "generic" implementation (i.e. without rewrite rules)
checkFMA_generic :: (RealFloat a, Show a, Arbitrary a, Random a) => String -> (Identity a -> Identity a -> Identity a -> Identity a) -> [(a, a, a, a)] -> Spec
checkFMA_generic name f cases = checkFMA name (coerce f) cases

spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "Double" $ do
    checkFMA         "fusedMultiplyAdd (default)"             fusedMultiplyAdd             casesForDouble
    checkFMA_generic "fusedMultiplyAdd (default, generic)"    fusedMultiplyAdd             casesForDouble
    checkFMA         "fusedMultiplyAdd (twoProduct)"          fusedMultiplyAdd_twoProduct  casesForDouble
    checkFMA_generic "fusedMultiplyAdd (twoProduct, generic)" fusedMultiplyAdd_twoProduct  casesForDouble
    checkFMA         "fusedMultiplyAdd (via Rational)"        fusedMultiplyAdd_viaRational casesForDouble
  describe "Float" $ do
    checkFMA         "fusedMultiplyAdd (default)"             fusedMultiplyAdd                casesForFloat
    checkFMA_generic "fusedMultiplyAdd (default, generic)"    fusedMultiplyAdd                casesForFloat
    checkFMA         "fusedMultiplyAdd (twoProduct)"          fusedMultiplyAdd_twoProduct     casesForFloat
    checkFMA_generic "fusedMultiplyAdd (twoProduct, generic)" fusedMultiplyAdd_twoProduct     casesForFloat
    checkFMA         "fusedMultiplyAdd (via Rational)"        fusedMultiplyAdd_viaRational    casesForFloat
    checkFMA         "fusedMultiplyAdd (via Double)"          fusedMultiplyAddFloat_viaDouble casesForFloat
#if defined(USE_FFI)
  describe "Extra" $ do
    describe "Double" $ do
      checkFMA "C fma" c_fma_double casesForDouble
    describe "Float" $ do
      checkFMA "C fmaf" c_fma_float casesForFloat
#endif
{-# NOINLINE spec #-}
