{-# LANGUAGE CPP #-}
module NextFloatSpec where
import           Data.Proxy
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Util (forAllFloats, sameFloatP)

#if defined(USE_FFI)

foreign import ccall unsafe "nextafter"
  c_nextafter_double :: Double -> Double -> Double
foreign import ccall unsafe "nextafterf"
  c_nextafter_float :: Float -> Float -> Float

class Fractional a => CFloat a where
  c_nextafter :: a -> a -> a

instance CFloat Double where
  c_nextafter = c_nextafter_double

instance CFloat Float where
  c_nextafter = c_nextafter_float

c_nextUp, c_nextDown, c_nextTowardZero :: (RealFloat a, CFloat a) => a -> a
c_nextUp x = c_nextafter x (1/0)
c_nextDown x = c_nextafter x (-1/0)
c_nextTowardZero x | isNegativeZero x = x
                   | otherwise = c_nextafter x 0

prop_nextUp_match :: (RealFloat a, CFloat a, Show a) => Proxy a -> a -> Property
prop_nextUp_match _ x = nextUp x `sameFloatP` c_nextUp x

prop_nextDown_match :: (RealFloat a, CFloat a, Show a) => Proxy a -> a -> Property
prop_nextDown_match _ x = nextDown x `sameFloatP` c_nextDown x

prop_nextTowardZero_match :: (RealFloat a, CFloat a, Show a) => Proxy a -> a -> Property
prop_nextTowardZero_match _ x = nextTowardZero x `sameFloatP` c_nextTowardZero x

#endif

isPositiveZero :: RealFloat a => a -> Bool
isPositiveZero x = x == 0 && not (isNegativeZero x)

prop_nextUp_nextDown :: (RealFloat a, Show a) => Proxy a -> a -> Property
prop_nextUp_nextDown _ x = x /= (-1/0) ==>
  let x' = nextUp (nextDown x)
  in x' `sameFloatP` x .||. (isPositiveZero x .&&. isNegativeZero x')

prop_nextDown_nextUp :: (RealFloat a, Show a) => Proxy a -> a -> Property
prop_nextDown_nextUp _ x = x /= (1/0) ==>
  let x' = nextDown (nextUp x)
  in x' `sameFloatP` x .||. (isNegativeZero x .&&. isPositiveZero x')

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
#if defined(USE_FFI)
    prop "nextUp vs C nextafter" $ forAllFloats $ prop_nextUp_match proxy
    prop "nextDown vs C nextafter" $ forAllFloats $ prop_nextDown_match proxy
    prop "nextTowardZero vs C nextafter" $ forAllFloats $ prop_nextTowardZero_match proxy
#endif
    prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ prop_nextUp_nextDown proxy
    prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ prop_nextDown_nextUp proxy

  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
#if defined(USE_FFI)
    prop "nextUp vs C nextafter" $ forAllFloats $ prop_nextUp_match proxy
    prop "nextDown vs C nextafter" $ forAllFloats $ prop_nextDown_match proxy
    prop "nextTowardZero vs C nextafter" $ forAllFloats $ prop_nextTowardZero_match proxy
#endif
    prop "nextUp . nextDown == id (unless -inf)" $ forAllFloats $ prop_nextUp_nextDown proxy
    prop "nextDown . nextUp == id (unless inf)" $ forAllFloats $ prop_nextDown_nextUp proxy
