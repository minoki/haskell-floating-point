{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE HexFloatLiterals #-}
module FMASpec where
import           Control.Monad
import           Data.Bits
import           Data.Coerce
import           Data.Functor.Identity
import           Numeric
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
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

fusedMultiplyAdd_generic :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_generic x y z = runIdentity (fusedMultiplyAdd (Identity x) (Identity y) (Identity z))

fusedMultiplyAdd_viaInteger :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_viaInteger x y z
  | isFinite x && isFinite y && isFinite z =
      let (mx,ex) = decodeFloat x -- x == mx * b^ex, mx==0 || b^(d-1) <= abs mx < b^d
          (my,ey) = decodeFloat y -- y == my * b^ey, my==0 || b^(d-1) <= abs my < b^d
          (mz,ez) = decodeFloat z -- z == mz * b^ez, mz==0 || b^(d-1) <= abs mz < b^d
          exy = ex + ey
          ee = min ez exy
          !2 = floatRadix x
      in case mx * my `shiftL` (exy - ee) + mz `shiftL` (ez - ee) of
           0 -> x * y + z
           m -> roundTiesToEven (encodeFloatR m ee)
  | isFinite x && isFinite y = z + z -- x * y is finite, but z is Infinity or NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN

fusedMultiplyAdd_viaRational :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_viaRational x y z
  | isFinite x && isFinite y && isFinite z =
      case toRational x * toRational y + toRational z of
        0 -> x * y + z
        r -> fromRational r
  | isFinite x && isFinite y = z + z -- x * is finite, but z is Infinity or NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN

casesForDouble :: [(Double, Double, Double, Double)]
casesForDouble =
  [ (0x1.af7da9fc47b3ep-1,     0x1p-1074,            -0x1p-1074, -0)
  , (0x1p512,                  0x1p512,              -0x1p1023,   0x1p1023)
  , (0x1.0000000000008p500,    0x1.1p500,             0x1p-1074,  0x1.1000000000009p1000)
  , (0x1.0000000000001p500,    0x1.8p500,            -0x1p-1074,  0x1.8000000000001p1000)
  , (0x1.ffffffc000000p512,    0x1.0000002p511,      -0x1p-1074,  0x1.fffffffffffffp1023) -- 0x1.ffffffc000000p512 * 0x1.0000002p511 == 0x1.fffffffffffff8p1023 (in Rational)
  , (-0x1.032ede48bbb28p-1022, 0x1.3cbc999ae14a8p-1, -0x1p-1074, -0x1.40accc50d63d2p-1023)
  , (0x1.ca903c622e5a6p-1022, 0x1.414a00c886a44p-1, 0x1.f1a8235fd56fep-1022, 0x1.88b4ec63db4f5p-1021)
  ]

casesForFloat :: [(Float, Float, Float, Float)]
casesForFloat =
  [ (16777215, 268435520, 63.5, 0x1.000002p52)
  , (0x1.84ae30p125, 0x1.6p-141,    0x1p-149,       0x1.0b37c2p-15)
  , (0x1.000010p50,  0x1.1p50,      0x1p-149,       0x1.100012p100)
  , (0x1.000002p50,  0x1.8p50,     -0x1p-149,       0x1.800002p100)
  , (0x1.83bd78p4,  -0x1.cp118,    -0x1.344108p-2, -0x1.5345cap123)
  , (0x1p-149,       0x1.88dd0cp-1, 0x1.081ffp-127, 0x1.081ff4p-127)
  , (0x1.d1a9dp-126, 0x1.594da4p-1, 0x1.343de4p-126, 0x1.3725b6p-125)
  , (-0x1.bf874ap4,  0x1.0fd36p3,  -0x1.959312p2,   -0x1.e7de54p7)
  , (0x1.ba7f24p5,   0x1.873d8p-1, -0x1.43edf4p-1,   0x1.4d1186p5)
  , (0x1.ce8eep-128, 0x1.9082ep124, -0x1.348fd8p4,  -0x1.31bc2cp4)
  , (0x1.fb449cp-126, -0x1.6d2ffp125, 0x1.94481p3,   0x1.670e14p3)
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

spec :: Spec
spec = modifyMaxSuccess (* 1000) $ do
  describe "Double" $ do
    checkFMA "fusedMultiplyAdd (default)"      fusedMultiplyAdd             casesForDouble
    checkFMA "fusedMultiplyAdd (generic)"      fusedMultiplyAdd_generic     casesForDouble
    checkFMA "fusedMultiplyAdd (via Rational)" fusedMultiplyAdd_viaRational casesForDouble
    checkFMA "fusedMultiplyAdd (via Integer)"  fusedMultiplyAdd_viaInteger  casesForDouble
  describe "Float" $ do
    checkFMA "fusedMultiplyAdd (default)"      fusedMultiplyAdd                casesForFloat
    checkFMA "fusedMultiplyAdd (generic)"      fusedMultiplyAdd_generic        casesForFloat
    checkFMA "fusedMultiplyAdd (via Rational)" fusedMultiplyAdd_viaRational    casesForFloat
    checkFMA "fusedMultiplyAdd (via Integer)"  fusedMultiplyAdd_viaInteger     casesForFloat
    checkFMA "fusedMultiplyAdd (via Double)"   fusedMultiplyAddFloat_viaDouble casesForFloat
#if defined(USE_FFI)
  describe "Extra" $ do
    describe "Double" $ do
      checkFMA "C fma" c_fma_double casesForDouble
    describe "Float" $ do
      checkFMA "C fmaf" c_fma_float casesForFloat
#endif
{-# NOINLINE spec #-}
