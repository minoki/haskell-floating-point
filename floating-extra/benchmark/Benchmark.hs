import           Numeric.Floating.Extra.IEEE
import Gauge.Main
import Data.Functor.Identity
import Data.Coerce
import GHC.Float (isDoubleFinite, isFloatFinite)

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

fusedMultiplyAddDouble_generic :: Double -> Double -> Double -> Double
fusedMultiplyAddDouble_generic = coerce (fusedMultiplyAdd :: Identity Double -> Identity Double -> Identity Double -> Identity Double)
fusedMultiplyAddFloat_generic :: Float -> Float -> Float -> Float
fusedMultiplyAddFloat_generic = coerce (fusedMultiplyAdd :: Identity Float -> Identity Float -> Identity Float -> Identity Float)

fusedMultiplyAddDouble_twoProduct_generic :: Double -> Double -> Double -> Double
fusedMultiplyAddDouble_twoProduct_generic = coerce (fusedMultiplyAdd_twoProduct :: Identity Double -> Identity Double -> Identity Double -> Identity Double)
fusedMultiplyAddFloat_twoProduct_generic :: Float -> Float -> Float -> Float
fusedMultiplyAddFloat_twoProduct_generic = coerce (fusedMultiplyAdd_twoProduct :: Identity Float -> Identity Float -> Identity Float -> Identity Float)

main :: IO ()
main = defaultMain
       [ bgroup "FMA"
         [ let arg = (1.0, 2.0, 3.0) :: (Double, Double, Double)
           in bgroup "Double"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell (default)" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (default, generic)" $ nf (\(x,y,z) -> fusedMultiplyAddDouble_generic x y z) arg
           , bench "Haskell (via Rational)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaRational x y z) arg
           , bench "Haskell (via Integer)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaInteger x y z) arg
           , bench "Haskell (TwoProduct)" $ nf (\(x,y,z) -> fusedMultiplyAdd_twoProduct x y z) arg
           , bench "Haskell (TwoProduct, generic)" $ nf (\(x,y,z) -> fusedMultiplyAddDouble_twoProduct_generic x y z) arg
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
         , let arg = (1.0, 2.0, 3.0) :: (Float, Float, Float)
           in bgroup "Float"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell (default)" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (default, generic)" $ nf (\(x,y,z) -> fusedMultiplyAddFloat_generic x y z) arg
           , bench "Haskell (via Rational)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaRational x y z) arg
           , bench "Haskell (via Integer)" $ nf (\(x,y,z) -> fusedMultiplyAdd_viaInteger x y z) arg
           , bench "Haskell (TwoProduct)" $ nf (\(x,y,z) -> fusedMultiplyAdd_twoProduct x y z) arg
           , bench "Haskell (TwoProduct, generic)" $ nf (\(x,y,z) -> fusedMultiplyAddFloat_twoProduct_generic x y z) arg
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
              , bench "Haskell (nonscaling)" $ nf (uncurry twoProduct_nonscaling) arg
              , bench "asm" $ nf (uncurry fastTwoProductDouble) arg
              ]
         , let arg :: (Float, Float)
               arg = (1.3 * 2^50, pi / 2^50)
           in bgroup "Float"
              [ bench "Haskell (default)" $ nf (uncurry twoProduct) arg
              , bench "Haskell (nonscaling)" $ nf (uncurry twoProduct_nonscaling) arg
              , bench "Haskell (via Double)" $ nf (uncurry twoProductFloat_viaDouble) arg
              , bench "asm" $ nf (uncurry fastTwoProductFloat) arg
              ]
         ]
       ]
