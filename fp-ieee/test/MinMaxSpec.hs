module MinMaxSpec where
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Proxy
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Numeric.Floating.IEEE.NaN (RealFloatNaN(..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Util

default ()

isQuietNaN :: RealFloatNaN a => a -> Bool
isQuietNaN x = isNaN x && not (isSignaling x)

prop_minimum :: RealFloatNaN a => Proxy a -> (a -> a -> a) -> Property
prop_minimum _ m =
  let sNaN = setPayloadSignaling 1
      qNaN = setPayload 1
  in conjoin
     [ counterexample "(1,3)" $ m 1 3 `sameFloatP` 1
     , counterexample "(1,-1)" $ m 1 (-1) `sameFloatP` (-1)
     , counterexample "(0,0)" $ m 0 0 `sameFloatP` 0
     , counterexample "(0,-0)" $ m 0 (-0) `sameFloatP` (-0)
     , counterexample "(-0,0)" $ m (-0) 0 `sameFloatP` (-0)
     , counterexample "(-0,-0)" $ m (-0) (-0) `sameFloatP` (-0)
     , counterexample "(sNaN,sNaN)" $ isQuietNaN (m sNaN sNaN)
     , counterexample "(sNaN,qNaN)" $ isQuietNaN (m sNaN qNaN)
     , counterexample "(qNaN,sNaN)" $ isQuietNaN (m qNaN sNaN)
     , counterexample "(qNaN,qNaN)" $ isQuietNaN (m qNaN qNaN)
     , counterexample "(sNaN,1.0)" $ isQuietNaN (m sNaN 1.0)
     , counterexample "(1.0,sNaN)" $ isQuietNaN (m 1.0 sNaN)
     , counterexample "(qNaN,1.0)" $ isQuietNaN (m qNaN 1.0)
     , counterexample "(1.0,qNaN)" $ isQuietNaN (m 1.0 qNaN)
     ]

prop_maximum :: RealFloatNaN a => Proxy a -> (a -> a -> a) -> Property
prop_maximum _ m =
  let sNaN = setPayloadSignaling 1
      qNaN = setPayload 1
  in conjoin
     [ counterexample "(1,3)" $ m 1 3 `sameFloatP` 3
     , counterexample "(1,-1)" $ m 1 (-1) `sameFloatP` 1
     , counterexample "(0,0)" $ m 0 0 `sameFloatP` 0
     , counterexample "(0,-0)" $ m 0 (-0) `sameFloatP` 0
     , counterexample "(-0,0)" $ m (-0) 0 `sameFloatP` 0
     , counterexample "(-0,-0)" $ m (-0) (-0) `sameFloatP` (-0)
     , counterexample "(sNaN,sNaN)" $ isQuietNaN (m sNaN sNaN)
     , counterexample "(sNaN,qNaN)" $ isQuietNaN (m sNaN qNaN)
     , counterexample "(qNaN,sNaN)" $ isQuietNaN (m qNaN sNaN)
     , counterexample "(qNaN,qNaN)" $ isQuietNaN (m qNaN qNaN)
     , counterexample "(sNaN,1.0)" $ isQuietNaN (m sNaN 1.0)
     , counterexample "(1.0,sNaN)" $ isQuietNaN (m 1.0 sNaN)
     , counterexample "(qNaN,1.0)" $ isQuietNaN (m qNaN 1.0)
     , counterexample "(1.0,qNaN)" $ isQuietNaN (m 1.0 qNaN)
     ]

prop_minimumNumber :: RealFloatNaN a => Proxy a -> (a -> a -> a) -> Property
prop_minimumNumber _ m =
  let sNaN = setPayloadSignaling 1
      qNaN = setPayload 1
  in conjoin
     [ counterexample "(1,3)" $ m 1 3 `sameFloatP` 1
     , counterexample "(1,-1)" $ m 1 (-1) `sameFloatP` (-1)
     , counterexample "(0,0)" $ m 0 0 `sameFloatP` 0
     , counterexample "(0,-0)" $ m 0 (-0) `sameFloatP` (-0)
     , counterexample "(-0,0)" $ m (-0) 0 `sameFloatP` (-0)
     , counterexample "(-0,-0)" $ m (-0) (-0) `sameFloatP` (-0)
     , counterexample "(sNaN,sNaN)" $ isQuietNaN (m sNaN sNaN)
     , counterexample "(sNaN,qNaN)" $ isQuietNaN (m sNaN qNaN)
     , counterexample "(qNaN,sNaN)" $ isQuietNaN (m qNaN sNaN)
     , counterexample "(qNaN,qNaN)" $ isQuietNaN (m qNaN qNaN)
     , counterexample "(sNaN,1.0)" $ m sNaN 1.0 `sameFloatP` 1.0
     , counterexample "(1.0,sNaN)" $ m 1.0 sNaN `sameFloatP` 1.0
     , counterexample "(qNaN,1.0)" $ m qNaN 1.0 `sameFloatP` 1.0
     , counterexample "(1.0,qNaN)" $ m 1.0 qNaN `sameFloatP` 1.0
     ]

prop_maximumNumber :: RealFloatNaN a => Proxy a -> (a -> a -> a) -> Property
prop_maximumNumber _ m =
  let sNaN = setPayloadSignaling 1
      qNaN = setPayload 1
  in conjoin
     [ counterexample "(1,3)" $ m 1 3 `sameFloatP` 3
     , counterexample "(1,-1)" $ m 1 (-1) `sameFloatP` 1
     , counterexample "(0,0)" $ m 0 0 `sameFloatP` 0
     , counterexample "(0,-0)" $ m 0 (-0) `sameFloatP` 0
     , counterexample "(-0,0)" $ m (-0) 0 `sameFloatP` 0
     , counterexample "(-0,-0)" $ m (-0) (-0) `sameFloatP` (-0)
     , counterexample "(sNaN,sNaN)" $ isQuietNaN (m sNaN sNaN)
     , counterexample "(sNaN,qNaN)" $ isQuietNaN (m sNaN qNaN)
     , counterexample "(qNaN,sNaN)" $ isQuietNaN (m qNaN sNaN)
     , counterexample "(qNaN,qNaN)" $ isQuietNaN (m qNaN qNaN)
     , counterexample "(sNaN,1.0)" $ m sNaN 1.0 `sameFloatP` 1.0
     , counterexample "(1.0,sNaN)" $ m 1.0 sNaN `sameFloatP` 1.0
     , counterexample "(qNaN,1.0)" $ m qNaN 1.0 `sameFloatP` 1.0
     , counterexample "(1.0,qNaN)" $ m 1.0 qNaN `sameFloatP` 1.0
     ]

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    prop "minimum'" $ prop_minimum proxy minimum'
    prop "minimum' (generic)" $ prop_minimum proxy (coerce (minimum' :: Identity Float -> Identity Float -> Identity Float))
    prop "minimumFloat" $ prop_minimum proxy minimumFloat
    prop "minimumNumber" $ prop_minimumNumber proxy minimumNumber
    prop "minimumNumber (generic)" $ prop_minimumNumber proxy (coerce (minimumNumber :: Identity Float -> Identity Float -> Identity Float))
    prop "minimumNumberFloat" $ prop_minimumNumber proxy minimumNumberFloat
    prop "maximum'" $ prop_maximum proxy maximum'
    prop "maximum' (generic)" $ prop_maximum proxy (coerce (maximum' :: Identity Float -> Identity Float -> Identity Float))
    prop "maximumFloat" $ prop_maximum proxy maximumFloat
    prop "maximumNumber" $ prop_maximumNumber proxy maximumNumber
    prop "maximumNumber (generic)" $ prop_maximumNumber proxy (coerce (maximumNumber :: Identity Float -> Identity Float -> Identity Float))
    prop "maximumNumberFloat" $ prop_maximumNumber proxy maximumNumberFloat
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "minimum'" $ prop_minimum proxy minimum'
    prop "minimum' (generic)" $ prop_minimum proxy (coerce (minimum' :: Identity Double -> Identity Double -> Identity Double))
    prop "minimumDouble" $ prop_minimum proxy minimumDouble
    prop "minimumNumber" $ prop_minimumNumber proxy minimumNumber
    prop "minimumNumber (generic)" $ prop_minimumNumber proxy (coerce (minimumNumber :: Identity Double -> Identity Double -> Identity Double))
    prop "minimumNumberDouble" $ prop_minimumNumber proxy minimumNumberDouble
    prop "maximum'" $ prop_maximum proxy maximum'
    prop "maximum' (generic)" $ prop_maximum proxy (coerce (maximum' :: Identity Double -> Identity Double -> Identity Double))
    prop "maximumDouble" $ prop_maximum proxy maximumDouble
    prop "maximumNumber" $ prop_maximumNumber proxy maximumNumber
    prop "maximumNumber (generic)" $ prop_maximumNumber proxy (coerce (maximumNumber :: Identity Double -> Identity Double -> Identity Double))
    prop "maximumNumberDouble" $ prop_maximumNumber proxy maximumNumberDouble
