{-# LANGUAGE HexFloatLiterals #-}
module FMASpec (spec) where
import           Numeric.Floating.Extra.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck -- (prop)
import           Test.QuickCheck
import           Util (sameFloat, sameFloatP, forAllFloats3)
import Numeric
import Control.Monad

foreign import ccall unsafe "fma"
  c_fma_double :: Double -> Double -> Double -> Double
foreign import ccall unsafe "fmaf"
  c_fma_float :: Float -> Float -> Float -> Float

-- The reference implementation
fusedMultiplyAdd_reference :: (RealFloat a, Show a) => a -> a -> a -> a
fusedMultiplyAdd_reference x y z
  | isFinite x && isFinite y && isFinite z = case toRational x * toRational y + toRational z of
                  0 -> x * y + z -- 'x * y + z' should be exact (no overflow / underflow)
                  r -> fromRational r
  | isFinite x && isFinite y = z
  | otherwise = x * y + z -- +-Infinity or NaN

prop_fma_match :: (RealFloat a, Show a) => a -> a -> a -> Property
prop_fma_match x y z = fusedMultiplyAdd x y z `sameFloatP` fusedMultiplyAdd_reference x y z

specialValuesForDouble :: [(Double, Double, Double, Double)]
specialValuesForDouble =
  [ (0x1.af7da9fc47b3ep-1,  0x1p-1074,       -0x1p-1074, -0)
  , (0x1p512,               0x1p512,         -0x1p1023,  0x1p1023)
  , (0x1.0000000000008p500, 0x1.1p500,        0x1p-1074, 0x1.1000000000009p1000)
  , (0x1.0000000000001p500, 0x1.8p500,       -0x1p-1074, 0x1.8000000000001p1000)
  , (0x1.ffffffc000000p512, 0x1.0000002p511, -0x1p-1074, 0x1.fffffffffffffp1023) -- 0x1.ffffffc000000p512 * 0x1.0000002p511 == 0x1.fffffffffffff8p1023 (in Rational)
  ]

specialValuesForFloat :: [(Float, Float, Float, Float)]
specialValuesForFloat =
  [ (16777215, 268435520, 63.5, 0x1.000002p52)
  , (0x1.84ae30p125, 0x1.6p-141, 0x1p-149, 0x1.0b37c2p-15)
  , (0x1.000010p50,  0x1.1p50,   0x1p-149, 0x1.100012p100)
  , (0x1.000002p50,  0x1.8p50,  -0x1p-149, 0x1.800002p100)
  , (0x1.83bd78p4,  -0x1.cp118, -0x1.344108p-2, -0x1.5345cap123)
  ]

testSpecialValues :: (RealFloat a, Show a) => String -> (a -> a -> a -> a) -> [(a, a, a, a)] -> Spec
testSpecialValues name f sp = forM_ sp $ \(a,b,c,result) -> do
  let label = showString name . showChar ' ' . showHFloat a . showChar ' ' . showHFloat b . showChar ' ' . showHFloat c . showString " should be " . showHFloat result $ ""
  it label $ f a b c `sameFloatP` result

spec :: Spec
spec = modifyMaxSuccess (* 10000) $ do
  describe "Double" $ do
    let fusedMultiplyAdd_generic :: Double -> Double -> Double -> Double
        fusedMultiplyAdd_generic = coerce (fusedMultiplyAdd :: Identity Double -> Identity Double -> Identity Double -> Identity Double)
        fusedMultiplyAdd_twoProduct_generic :: Double -> Double -> Double -> Double
        fusedMultiplyAdd_twoProduct_generic = coerce (fusedMultiplyAdd_twoProduct :: Identity Double -> Identity Double -> Identity Double -> Identity Double)
    -- prop "fusedMultiplyAdd vs C fma" $ forAllFloats3 (prop_fma_match :: Double -> Double -> Double -> Property)
    prop "fusedMultiplyAdd (twoProduct)" $ forAllFloats3 $ \x y z -> fusedMultiplyAdd_twoProduct x y z `sameFloatP` c_fma_double x y z
    testSpecialValues "c_fma_double" c_fma_double specialValuesForDouble
    testSpecialValues "fusedMultiplyAdd_viaRational" fusedMultiplyAdd_viaRational specialValuesForDouble
    testSpecialValues "fusedMultiplyAdd_twoProduct" fusedMultiplyAdd_twoProduct specialValuesForDouble
  describe "Float" $ do
    let fusedMultiplyAdd_generic :: Float -> Float -> Float -> Float
        fusedMultiplyAdd_generic = coerce (fusedMultiplyAdd :: Identity Float -> Identity Float -> Identity Float -> Identity Float)
        fusedMultiplyAdd_twoProduct_generic :: Float -> Float -> Float -> Float
        fusedMultiplyAdd_twoProduct_generic = coerce (fusedMultiplyAdd_twoProduct :: Identity Float -> Identity Float -> Identity Float -> Identity Float)
    -- prop "fusedMultiplyAdd vs C fma" $ forAllFloats3 (prop_fma_match :: Float -> Float -> Float -> Property)
    prop "fusedMultiplyAdd (twoProduct, generic)" $ forAllFloats3 $ \x y z -> fusedMultiplyAdd_twoProduct_generic x y z `sameFloatP` c_fma_float x y z
    prop "fusedMultiplyAdd (twoProduct)" $ forAllFloats3 $ \x y z -> fusedMultiplyAdd_twoProduct x y z `sameFloatP` c_fma_float x y z
    prop "fusedMultiplyAdd (via Double)" $ forAllFloats3 $ \x y z -> fusedMultiplyAddFloat_viaDouble x y z `sameFloatP` c_fma_float x y z
    testSpecialValues "c_fma_float" c_fma_float specialValuesForFloat
    testSpecialValues "fusedMultiplyAdd" fusedMultiplyAdd specialValuesForFloat
    testSpecialValues "fusedMultiplyAdd_generic" fusedMultiplyAdd_generic specialValuesForFloat
    testSpecialValues "fusedMultiplyAdd_viaRational" fusedMultiplyAdd_viaRational specialValuesForFloat
    testSpecialValues "fusedMultiplyAdd_twoProduct" fusedMultiplyAdd_twoProduct specialValuesForFloat
    testSpecialValues "fusedMultiplyAdd_twoProduct_generic" fusedMultiplyAdd_twoProduct_generic specialValuesForFloat
    testSpecialValues "fusedMultiplyAddFloat_viaDouble" fusedMultiplyAddFloat_viaDouble specialValuesForFloat
{-# NOINLINE spec #-}
