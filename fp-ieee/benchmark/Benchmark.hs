{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
import           Data.Bits
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Word
import           Gauge.Main
import           GHC.Float (isDoubleFinite, isFloatFinite)
import           Numeric
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
#if defined(USE_HALF)
import           Numeric.Half hiding (isZero)
import qualified Numeric.Half
#endif
#if defined(USE_FLOAT128)
import           Numeric.Float128 (Float128)
#endif

foreign import ccall unsafe "nextafter"
  c_nextafter_double :: Double -> Double -> Double
foreign import ccall unsafe "nextafterf"
  c_nextafter_float :: Float -> Float -> Float
foreign import ccall unsafe "fma"
  c_fma_double :: Double -> Double -> Double -> Double
foreign import ccall unsafe "fmaf"
  c_fma_float :: Float -> Float -> Float -> Float

class Fractional a => CFloat a where
  c_nextafter :: a -> a -> a
  c_fma :: a -> a -> a -> a

instance CFloat Double where
  c_nextafter = c_nextafter_double
  c_fma = c_fma_double

instance CFloat Float where
  c_nextafter = c_nextafter_float
  c_fma = c_fma_float

c_nextUp, c_nextDown :: (RealFloat a, CFloat a) => a -> a
c_nextUp x = c_nextafter x (1/0)
c_nextDown x = c_nextafter x (-1/0)

twoProduct_generic :: RealFloat a => a -> a -> (a, a)
twoProduct_generic x y = coerce (twoProduct (Identity x) (Identity y))

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

main :: IO ()
main = defaultMain
       [ bgroup "FMA"
         [ let arg = (1.0, 2.0, 3.0) :: (Double, Double, Double)
           in bgroup "Double"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell (default)" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (generic)" $ nf (\(x,y,z) -> fusedMultiplyAdd_generic x y z) arg
           , bench "Haskell (via Rational)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaRational x y z) arg
           , bench "Haskell (via Integer)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaInteger x y z) arg
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
         , let arg = (1.0, 2.0, 3.0) :: (Float, Float, Float)
           in bgroup "Float"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell (default)" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (generic)" $ nf (\(x,y,z) -> fusedMultiplyAdd_generic x y z) arg
           , bench "Haskell (via Rational)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaRational x y z) arg
           , bench "Haskell (via Integer)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaInteger x y z) arg
           , bench "Haskell (via Double)" $ nf (\(x,y,z) -> fusedMultiplyAddFloat_viaDouble x y z) arg
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
         ]
       , bgroup "isNormal"
         [ let arg = pi :: Double
           in bgroup "Double"
              [ bench "default" $ nf isNormal arg
              , bench "generic" $ nf (isNormal . Identity) arg
              ]
         , let arg = pi :: Float
           in bgroup "Float"
              [ bench "default" $ nf isNormal arg
              , bench "generic" $ nf (isNormal . Identity) arg
              ]
         ]
       , bgroup "isFinite"
         [ let arg = pi :: Double
           in bgroup "Double"
              [ bench "default" $ nf isFinite arg
              , bench "generic" $ nf (isFinite . Identity) arg
              , bench "GHC.Float.isDoubleFinite" $ nf isDoubleFinite arg
              ]
         , let arg = pi :: Float
           in bgroup "Float"
              [ bench "default" $ nf isFinite arg
              , bench "generic" $ nf (isFinite . Identity) arg
              , bench "GHC.Float.isFloatFinite" $ nf isFloatFinite arg
              ]
         ]
       , bgroup "twoProduct"
         [ let arg :: (Double, Double)
               arg = (1.3 * 2^500, pi / 2^500)
           in bgroup "Double"
              [ bench "Haskell (default)" $ nf (uncurry twoProduct) arg
              , bench "Haskell (generic)" $ nf (uncurry twoProduct_generic) arg
              , bench "Haskell (nonscaling)" $ nf (uncurry twoProduct_nonscaling) arg
#if defined(HAS_FAST_FMA)
              , bench "FMA" $ nf (uncurry twoProductDouble) arg
#endif
              ]
         , let arg :: (Float, Float)
               arg = (1.3 * 2^50, pi / 2^50)
           in bgroup "Float"
              [ bench "Haskell (default)" $ nf (uncurry twoProduct) arg
              , bench "Haskell (generic)" $ nf (uncurry twoProduct_generic) arg
              , bench "Haskell (nonscaling)" $ nf (uncurry twoProduct_nonscaling) arg
              , bench "Haskell (via Double)" $ nf (uncurry twoProductFloat_viaDouble) arg
#if defined(HAS_FAST_FMA)
              , bench "FMA" $ nf (uncurry twoProductFloat) arg
#endif
              ]
         ]
       , bgroup "fromInteger"
         [ let x = 418237418 * 2^80 + 4811 * 2^32 + 1412
             in bgroup "large"
           [ bgroup "Double"
             [ bench "stock" $ nf (fromInteger :: Integer -> Double) x
             , bench "fromIntegerTiesToEven" $ nf (fromIntegerTiesToEven :: Integer -> Double) x
             , bench "fromIntegerTiesToAway" $ nf (fromIntegerTiesToAway :: Integer -> Double) x
             , bench "fromIntegerTowardPositive" $ nf (fromIntegerTowardPositive :: Integer -> Double) x
             , bench "fromIntegerTowardNegative" $ nf (fromIntegerTowardNegative :: Integer -> Double) x
             , bench "fromIntegerTowardZero" $ nf (fromIntegerTowardZero :: Integer -> Double) x
             ]
           , bgroup "Float"
             [ bench "stock" $ nf (fromInteger :: Integer -> Float) x
             , bench "fromIntegerTiesToEven" $ nf (fromIntegerTiesToEven :: Integer -> Float) x
             , bench "fromIntegerTiesToAway" $ nf (fromIntegerTiesToAway :: Integer -> Float) x
             , bench "fromIntegerTowardPositive" $ nf (fromIntegerTowardPositive :: Integer -> Float) x
             , bench "fromIntegerTowardNegative" $ nf (fromIntegerTowardNegative :: Integer -> Float) x
             , bench "fromIntegerTowardZero" $ nf (fromIntegerTowardZero :: Integer -> Float) x
             ]
           ]
         , let x = 3 * 2^19 + 4811 * 2^7 + 1412
           in bgroup "small"
           [ bgroup "Double"
             [ bench "stock" $ nf (fromInteger :: Integer -> Double) x
             , bench "fromIntegerTiesToEven" $ nf (fromIntegerTiesToEven :: Integer -> Double) x
             , bench "fromIntegerTiesToAway" $ nf (fromIntegerTiesToAway :: Integer -> Double) x
             , bench "fromIntegerTowardPositive" $ nf (fromIntegerTowardPositive :: Integer -> Double) x
             , bench "fromIntegerTowardNegative" $ nf (fromIntegerTowardNegative :: Integer -> Double) x
             , bench "fromIntegerTowardZero" $ nf (fromIntegerTowardZero :: Integer -> Double) x
             ]
           , bgroup "Float"
             [ bench "stock" $ nf (fromInteger :: Integer -> Float) x
             , bench "fromIntegerTiesToEven" $ nf (fromIntegerTiesToEven :: Integer -> Float) x
             , bench "fromIntegerTiesToAway" $ nf (fromIntegerTiesToAway :: Integer -> Float) x
             , bench "fromIntegerTowardPositive" $ nf (fromIntegerTowardPositive :: Integer -> Float) x
             , bench "fromIntegerTowardNegative" $ nf (fromIntegerTowardNegative :: Integer -> Float) x
             , bench "fromIntegerTowardZero" $ nf (fromIntegerTowardZero :: Integer -> Float) x
             ]
           ]
         ]
       , bgroup "fromIntegral"
         [ bgroup "Word64"
           [ let x = 0xdead_beef_1234_7777 :: Word64
             in bgroup "large"
                [ bgroup "Double"
                  [ bench "stock" $ nf (fromIntegral :: Word64 -> Double) x
                  , bench "fromIntegralTiesToEven" $ nf (fromIntegralTiesToEven :: Word64 -> Double) x
                  , bench "fromIntegralTiesToAway" $ nf (fromIntegralTiesToAway :: Word64 -> Double) x
                  , bench "fromIntegralTowardPositive" $ nf (fromIntegralTowardPositive :: Word64 -> Double) x
                  , bench "fromIntegralTowardNegative" $ nf (fromIntegralTowardNegative :: Word64 -> Double) x
                  , bench "fromIntegralTowardZero" $ nf (fromIntegralTowardZero :: Word64 -> Double) x
                  ]
                , bgroup "Float"
                  [ bench "stock" $ nf (fromIntegral :: Word64 -> Float) x
                  , bench "fromIntegralTiesToEven" $ nf (fromIntegralTiesToEven :: Word64 -> Float) x
                  , bench "fromIntegralTiesToAway" $ nf (fromIntegralTiesToAway :: Word64 -> Float) x
                  , bench "fromIntegralTowardPositive" $ nf (fromIntegralTowardPositive :: Word64 -> Float) x
                  , bench "fromIntegralTowardNegative" $ nf (fromIntegralTowardNegative :: Word64 -> Float) x
                  , bench "fromIntegralTowardZero" $ nf (fromIntegralTowardZero :: Word64 -> Float) x
                  ]
                ]
           , let x = 0x14_7777 :: Word64
             in bgroup "small"
                [ bgroup "Double"
                  [ bench "stock" $ nf (fromIntegral :: Word64 -> Double) x
                  , bench "fromIntegralTiesToEven" $ nf (fromIntegralTiesToEven :: Word64 -> Double) x
                  , bench "fromIntegralTiesToAway" $ nf (fromIntegralTiesToAway :: Word64 -> Double) x
                  , bench "fromIntegralTowardPositive" $ nf (fromIntegralTowardPositive :: Word64 -> Double) x
                  , bench "fromIntegralTowardNegative" $ nf (fromIntegralTowardNegative :: Word64 -> Double) x
                  , bench "fromIntegralTowardZero" $ nf (fromIntegralTowardZero :: Word64 -> Double) x
                  ]
                , bgroup "Float"
                  [ bench "stock" $ nf (fromIntegral :: Word64 -> Float) x
                  , bench "fromIntegralTiesToEven" $ nf (fromIntegralTiesToEven :: Word64 -> Float) x
                  , bench "fromIntegralTiesToAway" $ nf (fromIntegralTiesToAway :: Word64 -> Float) x
                  , bench "fromIntegralTowardPositive" $ nf (fromIntegralTowardPositive :: Word64 -> Float) x
                  , bench "fromIntegralTowardNegative" $ nf (fromIntegralTowardNegative :: Word64 -> Float) x
                  , bench "fromIntegralTowardZero" $ nf (fromIntegralTowardZero :: Word64 -> Float) x
                  ]
                ]
           ]
         ]
       , bgroup "fromRational"
         [ let x = (418237418 * 2^80 + 4811 * 2^32 + 1412) / (2234321954 * 2^75 + 2345234566) :: Rational
           in bgroup "large/large"
              [ bgroup "Double"
                [ bench "stock" $ nf (fromRational :: Rational -> Double) x
                , bench "fromRationalTiesToEven" $ nf (fromRationalTiesToEven :: Rational -> Double) x
                , bench "fromRationalTiesToAway" $ nf (fromRationalTiesToAway :: Rational -> Double) x
                , bench "fromRationalTowardPositive" $ nf (fromRationalTowardPositive :: Rational -> Double) x
                , bench "fromRationalTowardNegative" $ nf (fromRationalTowardNegative :: Rational -> Double) x
                , bench "fromRationalTowardZero" $ nf (fromRationalTowardZero :: Rational -> Double) x
                ]
              , bgroup "Float"
                [ bench "stock" $ nf (fromRational :: Rational -> Float) x
                , bench "fromRationalTiesToEven" $ nf (fromRationalTiesToEven :: Rational -> Float) x
                , bench "fromRationalTiesToAway" $ nf (fromRationalTiesToAway :: Rational -> Float) x
                , bench "fromRationalTowardPositive" $ nf (fromRationalTowardPositive :: Rational -> Float) x
                , bench "fromRationalTowardNegative" $ nf (fromRationalTowardNegative :: Rational -> Float) x
                , bench "fromRationalTowardZero" $ nf (fromRationalTowardZero :: Rational -> Float) x
                ]
              ]
         , let x = 355 / 113 :: Rational
           in bgroup "small/small"
              [ bgroup "Double"
                [ bench "stock" $ nf (fromRational :: Rational -> Double) x
                , bench "fromRationalTiesToEven" $ nf (fromRationalTiesToEven :: Rational -> Double) x
                , bench "fromRationalTiesToAway" $ nf (fromRationalTiesToAway :: Rational -> Double) x
                , bench "fromRationalTowardPositive" $ nf (fromRationalTowardPositive :: Rational -> Double) x
                , bench "fromRationalTowardNegative" $ nf (fromRationalTowardNegative :: Rational -> Double) x
                , bench "fromRationalTowardZero" $ nf (fromRationalTowardZero :: Rational -> Double) x
                ]
              , bgroup "Float"
                [ bench "stock" $ nf (fromRational :: Rational -> Float) x
                , bench "fromRationalTiesToEven" $ nf (fromRationalTiesToEven :: Rational -> Float) x
                , bench "fromRationalTiesToAway" $ nf (fromRationalTiesToAway :: Rational -> Float) x
                , bench "fromRationalTowardPositive" $ nf (fromRationalTowardPositive :: Rational -> Float) x
                , bench "fromRationalTowardNegative" $ nf (fromRationalTowardNegative :: Rational -> Float) x
                , bench "fromRationalTowardZero" $ nf (fromRationalTowardZero :: Rational -> Float) x
                ]
              ]
         , let x = 0x1.deafbeefcafec0ffeep100 :: Rational
           in bgroup "binary"
              [ bgroup "Double"
                [ bench "stock" $ nf (fromRational :: Rational -> Double) x
                , bench "fromRationalTiesToEven" $ nf (fromRationalTiesToEven :: Rational -> Double) x
                , bench "fromRationalTiesToAway" $ nf (fromRationalTiesToAway :: Rational -> Double) x
                , bench "fromRationalTowardPositive" $ nf (fromRationalTowardPositive :: Rational -> Double) x
                , bench "fromRationalTowardNegative" $ nf (fromRationalTowardNegative :: Rational -> Double) x
                , bench "fromRationalTowardZero" $ nf (fromRationalTowardZero :: Rational -> Double) x
                ]
              , bgroup "Float"
                [ bench "stock" $ nf (fromRational :: Rational -> Float) x
                , bench "fromRationalTiesToEven" $ nf (fromRationalTiesToEven :: Rational -> Float) x
                , bench "fromRationalTiesToAway" $ nf (fromRationalTiesToAway :: Rational -> Float) x
                , bench "fromRationalTowardPositive" $ nf (fromRationalTowardPositive :: Rational -> Float) x
                , bench "fromRationalTowardNegative" $ nf (fromRationalTowardNegative :: Rational -> Float) x
                , bench "fromRationalTowardZero" $ nf (fromRationalTowardZero :: Rational -> Float) x
                ]
              ]
         ]
       , bgroup "encodeFloat"
         [ let arg = (0xcafe_0000_abcd_7777, -25) :: (Integer, Int)
           in bgroup "Double"
              [ bench "stock" $ nf (uncurry encodeFloat :: (Integer, Int) -> Double) arg
              , bench "encodeFloatTiesToEven" $ nf (uncurry encodeFloatTiesToEven :: (Integer, Int) -> Double) arg
              , bench "encodeFloatTiesToAway" $ nf (uncurry encodeFloatTiesToAway :: (Integer, Int) -> Double) arg
              , bench "encodeFloatTowardPositive" $ nf (uncurry encodeFloatTowardPositive :: (Integer, Int) -> Double) arg
              , bench "encodeFloatTowardNegative" $ nf (uncurry encodeFloatTowardNegative :: (Integer, Int) -> Double) arg
              , bench "encodeFloatTowardZero" $ nf (uncurry encodeFloatTowardZero :: (Integer, Int) -> Double) arg
              ]
         , let arg = (0xcafe_0000_abcd_7777, -25) :: (Integer, Int)
           in bgroup "Float"
              [ bench "stock" $ nf (uncurry encodeFloat :: (Integer, Int) -> Float) arg
              , bench "encodeFloatTiesToEven" $ nf (uncurry encodeFloatTiesToEven :: (Integer, Int) -> Float) arg
              , bench "encodeFloatTiesToAway" $ nf (uncurry encodeFloatTiesToAway :: (Integer, Int) -> Float) arg
              , bench "encodeFloatTowardPositive" $ nf (uncurry encodeFloatTowardPositive :: (Integer, Int) -> Float) arg
              , bench "encodeFloatTowardNegative" $ nf (uncurry encodeFloatTowardNegative :: (Integer, Int) -> Float) arg
              , bench "encodeFloatTowardZero" $ nf (uncurry encodeFloatTowardZero :: (Integer, Int) -> Float) arg
              ]
         ]
       , bgroup "minimum"
         [ bgroup "Double"
           [ let arg = (pi, -2.3) :: (Double, Double)
             in bgroup "(pi, -2.3)"
                [ bench "stock" $ whnf (uncurry min) arg
                , bench "minimum" $ whnf (uncurry minimum') arg
                , bench "minimumNumber" $ whnf (uncurry minimumNumber) arg
                , bench "minimumMagnitude" $ whnf (uncurry minimumMagnitude) arg
                , bench "minimumMagnitudeNumber" $ whnf (uncurry minimumMagnitudeNumber) arg
                , bench "minimum (specialized)" $ whnf (uncurry minimumDouble) arg
                , bench "minimumNumber (specialized)" $ whnf (uncurry minimumNumberDouble) arg
                ]
           , let arg = (0, -0) :: (Double, Double)
             in bgroup "(0, -0)"
                [ bench "stock" $ whnf (uncurry min) arg
                , bench "minimum" $ whnf (uncurry minimum') arg
                , bench "minimumNumber" $ whnf (uncurry minimumNumber) arg
                , bench "minimumMagnitude" $ whnf (uncurry minimumMagnitude) arg
                , bench "minimumMagnitudeNumber" $ whnf (uncurry minimumMagnitudeNumber) arg
                , bench "minimum (specialized)" $ whnf (uncurry minimumDouble) arg
                , bench "minimumNumber (specialized)" $ whnf (uncurry minimumNumberDouble) arg
                ]
           ]
         ]
       , bgroup "canonicalize"
         [ let x = 0 / 0 :: Float
           in bgroup "Float"
           [ bench "Haskell" $ whnf canonicalize x
           , bench "Haskell (generic)" $ whnf canonicalize (Identity x)
           , bench "C" $ whnf canonicalizeFloat x
           , bench "identity" $ whnf id x
           ]
         , let x = 0 / 0 :: Double
           in bgroup "Double"
           [ bench "Haskell" $ whnf canonicalize x
           , bench "Haskell (generic)" $ whnf canonicalize (Identity x)
           , bench "C" $ whnf canonicalizeDouble x
           , bench "identity" $ whnf id x
           ]
         ]
       , bgroup "nextUp"
         [ let cases = [0,1,0x1.ffff_ffff_ffff_fp200] :: [Double]
           in bgroup "Double"
              [ bgroup "C"
                [ bench (showHFloat x "") $ nf c_nextUp x | x <- cases ]
              , bgroup "Haskell"
                [ bench (showHFloat x "") $ nf nextUp x | x <- cases ]
              , bgroup "Haskell (generic)"
                [ bench (showHFloat x "") $ nf nextUp (Identity x) | x <- cases ]
              ]
         , let cases = [0,1,0x1.fffffep100] :: [Float]
           in bgroup "Float"
              [ bgroup "C"
                [ bench (showHFloat x "") $ nf c_nextUp x | x <- cases ]
              , bgroup "Haskell"
                [ bench (showHFloat x "") $ nf nextUp x | x <- cases ]
              , bgroup "Haskell (generic)"
                [ bench (showHFloat x "") $ nf nextUp (Identity x) | x <- cases ]
              ]
         ]
       , bgroup "nextDown"
         [ let cases = [0,1,0x1.ffff_ffff_ffff_fp200] :: [Double]
           in bgroup "Double"
              [ bgroup "C"
                [ bench (showHFloat x "") $ nf c_nextDown x | x <- cases ]
              , bgroup "Haskell"
                [ bench (showHFloat x "") $ nf nextDown x | x <- cases ]
              , bgroup "Haskell (generic)"
                [ bench (showHFloat x "") $ nf nextDown (Identity x) | x <- cases ]
              ]
         , let cases = [0,1,0x1.fffffep100] :: [Float]
           in bgroup "Float"
              [ bgroup "C"
                [ bench (showHFloat x "") $ nf c_nextDown x | x <- cases ]
              , bgroup "Haskell"
                [ bench (showHFloat x "") $ nf nextDown x | x <- cases ]
              , bgroup "Haskell (generic)"
                [ bench (showHFloat x "") $ nf nextDown (Identity x) | x <- cases ]
              ]
         ]
#if defined(USE_HALF)
       , bgroup "Half"
         [ bgroup "from Half"
           [ let x = 1.3 :: Half
             in bgroup "to Float"
                [ bench "half" $ nf fromHalf x
#if defined(HAS_FAST_HALF_CONVERSION)
                , bench "C impl" $ nf halfToFloat x
#endif
                , bench "realToFrac" $ nf (realToFrac :: Half -> Float) x
                , bench "realFloatToFrac" $ nf (realFloatToFrac :: Half -> Float) x
                ]
           , let x = 1.3 :: Half
             in bgroup "to Double"
                [
#if defined(HAS_FAST_HALF_CONVERSION)
                  bench "C impl" $ nf halfToDouble x ,
#endif
                  bench "realToFrac" $ nf (realToFrac :: Half -> Double) x
                , bench "realFloatToFrac" $ nf (realFloatToFrac :: Half -> Double) x
                ]
           ]
         , bgroup "to Half"
           [ let x = 1.3 :: Float
             in bgroup "from Float"
                [ bench "half" $ nf toHalf x
#if defined(HAS_FAST_HALF_CONVERSION)
                , bench "C impl" $ nf floatToHalf x
#endif
                , bench "realToFrac" $ nf (realToFrac :: Float -> Half) x
                , bench "realFloatToFrac" $ nf (realFloatToFrac :: Float -> Half) x
                ]
           , let x = 1.3 :: Double
             in bgroup "from Double"
                [
#if defined(HAS_FAST_HALF_CONVERSION)
                  bench "C impl" $ nf doubleToHalf x ,
#endif
                  bench "realToFrac" $ nf (realToFrac :: Double -> Half) x
                , bench "realFloatToFrac" $ nf (realFloatToFrac :: Double -> Half) x
                ]
           ]
         , let arg = pi :: Half
           in bgroup "isNormal"
              [ bench "default" $ nf isNormal arg
              , bench "generic" $ nf (isNormal . Identity) arg
              ]
         , let arg = pi :: Half
           in bgroup "isFinite"
              [ bench "default" $ nf isFinite arg
              , bench "generic" $ nf (isFinite . Identity) arg
              ]
         , let arg = -0 :: Half
           in bgroup "isZero"
              [ bench "default" $ nf isZero arg
              , bench "generic" $ nf (isZero . Identity) arg
              , bench "Numeric.Half.isZero" $ nf Numeric.Half.isZero arg
              ]
         ]
#endif
#if defined(USE_FLOAT128)
       , bgroup "Float128"
         [ bgroup "nextUp"
           [ bench "default" $ whnf nextUp (1.23 :: Float128)
           , bench "generic" $ whnf (nextUp . Identity) (1.23 :: Float128)
           ]
         , bgroup "nextDown"
           [ bench "default" $ whnf nextDown (1.23 :: Float128)
           , bench "generic" $ whnf (nextDown . Identity) (1.23 :: Float128)
           ]
         , bgroup "nextTowardZero"
           [ bench "default" $ whnf nextTowardZero (1.23 :: Float128)
           , bench "generic" $ whnf (nextTowardZero . Identity) (1.23 :: Float128)
           ]
         , bgroup "isNormal"
           [ bench "default" $ whnf isNormal (1.23 :: Float128)
           , bench "generic" $ whnf (isNormal . Identity) (1.23 :: Float128)
           ]
         , bgroup "isFinite"
           [ bench "default" $ whnf isFinite (1.23 :: Float128)
           , bench "generic" $ whnf (isFinite . Identity) (1.23 :: Float128)
           ]
         , bgroup "classify"
           [ bench "default" $ whnf classify (1.23 :: Float128)
           , bench "generic" $ whnf (classify . Identity) (1.23 :: Float128)
           ]
         , bgroup "isMantissaEven"
           [ bench "default" $ whnf isMantissaEven (1.23 :: Float128)
           , bench "generic" $ whnf (isMantissaEven . Identity) (1.23 :: Float128)
           ]
         , bgroup "roundAway"
           [ bench "default" $ whnf roundAway' (1.23 :: Float128)
           , bench "generic" $ whnf (roundAway' . Identity) (1.23 :: Float128)
           , bench "default (as Integer)" $ whnf (roundAway :: Float128 -> Integer) (1.23 :: Float128)
           , bench "generic (as Integer)" $ whnf ((roundAway :: Identity Float128 -> Integer) . Identity) (1.23 :: Float128)
           ]
         , bgroup "floor"
           [ bench "default" $ whnf floor' (1.23 :: Float128)
           , bench "generic" $ whnf (floor' . Identity) (1.23 :: Float128)
           , bench "default (as Integer)" $ whnf (floor :: Float128 -> Integer) (1.23 :: Float128)
           , bench "generic (as Integer)" $ whnf ((floor :: Identity Float128 -> Integer) . Identity) (1.23 :: Float128)
           ]
         ]
#endif
       ]
