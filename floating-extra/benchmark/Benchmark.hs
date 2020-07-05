import           Numeric.Floating.Extra.IEEE
import Gauge.Main

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

main :: IO ()
main = defaultMain
       [ bgroup "FMA"
         [ let arg = (1.0, 2.0, 3.0) :: (Double, Double, Double)
           in bgroup "Double"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell (via Rational)" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (TwoProduct)" $ nf (\(x,y,z) -> fusedMultiplyAdd_twoProduct (x) (y) (z)) arg
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
         , let arg = (1.0, 2.0, 3.0) :: (Float, Float, Float)
           in bgroup "Float"
           [ bench "C" $ nf (\(x,y,z) -> c_fma x y z) arg
           , bench "Haskell (via Rational)" $ nf (\(x,y,z) -> fusedMultiplyAdd x y z) arg
           , bench "Haskell (TwoProduct)" $ nf (\(x,y,z) -> fusedMultiplyAdd_twoProduct (x) (y) (z)) arg
           , bench "Haskell (via Double)" $ nf (\(x,y,z) -> fusedMultiplyAddFloat_viaDouble x y z) arg
           , bench "non-fused" $ nf (\(x,y,z) -> x * y + z) arg
           ]
         ]
       ]
