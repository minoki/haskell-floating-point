{-# LANGUAGE HexFloatLiterals #-}
module FMASpec (spec) where
import           Numeric.Floating.Extra.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck -- (prop)
import           Test.QuickCheck
import           Util (sameFloat, sameFloatP, forAllFloats3)

foreign import ccall unsafe "fma"
  c_fma_double :: Double -> Double -> Double -> Double
foreign import ccall unsafe "fmaf"
  c_fma_float :: Float -> Float -> Float -> Float

class Fractional a => CFloat a where
  c_fma :: a -> a -> a -> a

instance CFloat Double where
  c_fma = c_fma_double

instance CFloat Float where
  c_fma = c_fma_float

fusedMultiplyAdd_viaRational :: (RealFloat a, Show a) => a -> a -> a -> a
fusedMultiplyAdd_viaRational x y z
  | isInfinite z && isFinite x && isFinite y = z
  | isNaN x || isNaN y || isNaN z || isInfinite x || isInfinite y || isInfinite z = x * y + z
  | otherwise = case toRational x * toRational y + toRational z of
                  0 | isNegativeZero (x * y + z) -> -0
                  r -> fromRational r

prop_fma_match :: (RealFloat a, CFloat a, Show a) => a -> a -> a -> Property
prop_fma_match x y z = fusedMultiplyAdd x y z `sameFloatP` c_fma x y z

spec :: Spec
spec = modifyMaxSuccess (* 10000) $ do
  describe "Double" $ do
    -- prop "fusedMultiplyAdd vs C fma" $ forAllFloats3 (prop_fma_match :: Double -> Double -> Double -> Property)
    prop "fusedMultiplyAdd (twoProduct) vs C fma" $ forAllFloats3 $ \x y z -> fusedMultiplyAdd_twoProduct x y z `sameFloatP` c_fma_double x y z
    describe "special case" $ do
      it "twoProduct" $ fusedMultiplyAdd_twoProduct 0x1.af7da9fc47b3ep-1 0x1p-1074 (-0x1p-1074) `sameFloatP` (-0)
  describe "Float" $ do
    -- prop "fusedMultiplyAdd vs C fma" $ forAllFloats3 (prop_fma_match :: Float -> Float -> Float -> Property)
    prop "fusedMultiplyAdd (twoProduct) vs C fma" $ forAllFloats3 $ \x y z -> fusedMultiplyAdd_twoProduct x y z `sameFloatP` c_fma_float x y z
    -- prop "fusedMultiplyAdd (via Double) vs C fma" $ forAllFloats3 $ \x y z -> fusedMultiplyAddFloat_viaDouble x y z `sameFloatP` c_fma_float x y z
    describe "special case" $ do
      it "generic" $ (fusedMultiplyAdd 16777215 268435520 63.5 :: Float) `sameFloat` 0x1.000002p52
      it "twoProduct" $ (fusedMultiplyAdd_twoProduct 16777215 268435520 63.5 :: Float) `sameFloat` 0x1.000002p52
      it "twoProduct" $ (c_fma_float 16777215 268435520 63.5 :: Float) `sameFloat` 0x1.000002p52
      it "twoProduct" $ (fusedMultiplyAdd_twoProduct 0x1.84ae3p125 0x1.6p-141 0x1p-149 :: Float) `sameFloat` (c_fma_float 0x1.84ae3p125 0x1.6p-141 0x1p-149 :: Float)
      -- it "via Double" $ (fusedMultiplyAddFloat_viaDouble 16777215 268435520 63.5 :: Float) `sameFloat` 0x1.000002p52
      -- fusedMultiplyAdd_twoProduct @Float 0x1.83bd78p4 (-0x1.cp118) (-0x1.344108p-2)
{-# NOINLINE spec #-}
