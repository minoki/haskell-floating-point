module RoundToIntegralSpec where
import           Data.Proxy
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

prop_roundToIntegral :: (RealFloat a, Show a) => Proxy a -> a -> Property
prop_roundToIntegral _ x = isFinite x ==>
  let tiesToEven = round' x
      tiesToEvenInt = round x :: Integer
      tiesToAway = roundAway' x
      tiesToAwayInt = roundAway x :: Integer
      towardPositive = ceiling' x
      towardPositiveInt = ceiling x :: Integer
      towardNegative = floor' x
      towardNegativeInt = floor x :: Integer
      towardZero = truncate' x
      towardZeroInt = truncate x :: Integer
      sameInteger f i = round f === i .&&. f === fromInteger i
  in conjoin
     [ counterexample "tiesToEven" $ isFinite tiesToEven .&&. sameInteger tiesToEven tiesToEvenInt
     , counterexample "tiesToAway" $ isFinite tiesToAway .&&. sameInteger tiesToAway tiesToAwayInt
     , counterexample "towardPositive" $ isFinite towardPositive .&&. sameInteger towardPositive towardPositiveInt
     , counterexample "towardNegative" $ isFinite towardNegative .&&. sameInteger towardNegative towardNegativeInt
     , counterexample "towardZero" $ isFinite towardZero .&&. sameInteger towardZero towardZeroInt
     , counterexample "towardNegative <= original value" $ towardNegative <= x
     , counterexample "towardNegative <= tiesToEven" $ towardNegative <= tiesToEven
     , counterexample "towardNegative <= tiesToAway" $ towardNegative <= tiesToAway
     , counterexample "towardNegative <= towardPositive" $ towardNegative <= towardPositive
     , counterexample "towardNegative <= towardZero" $ towardNegative <= towardZero
     , counterexample "original value <= towardPositive" $ x <= towardPositive
     , counterexample "tiesToEven <= towardPositive" $ tiesToEven <= towardPositive
     , counterexample "tiesToAway <= towardPositive" $ tiesToAway <= towardPositive
     , counterexample "towardZero <= towardPositive" $ towardZero <= towardPositive
     , counterexample "abs towardZero <= abs (original value)" $ abs towardZero <= abs x
     , counterexample "abs towardZero <= abs tiesToEven" $ abs towardZero <= abs tiesToEven
     , counterexample "abs towardZero <= abs tiesToAway" $ abs towardZero <= abs tiesToAway
     , counterexample "abs towardZero <= abs towardPositive" $ abs towardZero <= abs towardPositive
     , counterexample "abs towardZero <= abs towardNegative" $ abs towardZero <= abs towardNegative
     ]

data RoundResult a = RoundResult { resultTiesToEven     :: a
                                 , resultTiesToAway     :: a
                                 , resultTowardPositive :: a
                                 , resultTowardNegative :: a
                                 , resultTowardZero     :: a
                                 }

checkBehavior :: RealFloat a => Proxy a -> a -> RoundResult a -> RoundResult Integer -> Spec
checkBehavior _ x result resultI = do
  it "tiesToEven" $ round' x `sameFloatP` resultTiesToEven result
  it "tiesToEven (Integer)" $ round x `shouldBe` resultTiesToEven resultI
  it "tiesToAway" $ roundAway' x `sameFloatP` resultTiesToAway result
  it "tiesToAway (Integer)" $ roundAway x `shouldBe` resultTiesToAway resultI
  it "ceiling" $ ceiling' x `sameFloatP` resultTowardPositive result
  it "ceiling (Integer)" $ ceiling x `shouldBe` resultTowardPositive resultI
  it "floor" $ floor' x `sameFloatP` resultTowardNegative result
  it "floor (Integer)" $ floor x `shouldBe` resultTowardNegative resultI
  it "truncate" $ truncate' x `sameFloatP` resultTowardZero result
  it "truncate (Integer)" $ truncate x `shouldBe` resultTowardZero resultI

checkCases :: RealFloat a => Proxy a -> Spec
checkCases proxy = do
  describe "0.5" $ checkBehavior proxy 0.5
    RoundResult { resultTiesToEven = 0.0
                , resultTiesToAway = 1.0
                , resultTowardPositive = 1.0
                , resultTowardNegative = 0.0
                , resultTowardZero = 0.0
                }
    RoundResult { resultTiesToEven = 0
                , resultTiesToAway = 1
                , resultTowardPositive = 1
                , resultTowardNegative = 0
                , resultTowardZero = 0
                }
  describe "0.25" $ checkBehavior proxy 0.25
    RoundResult { resultTiesToEven = 0.0
                , resultTiesToAway = 0.0
                , resultTowardPositive = 1.0
                , resultTowardNegative = 0.0
                , resultTowardZero = 0.0
                }
    RoundResult { resultTiesToEven = 0
                , resultTiesToAway = 0
                , resultTowardPositive = 1
                , resultTowardNegative = 0
                , resultTowardZero = 0
                }
  describe "-0.25" $ checkBehavior proxy (-0.25)
    RoundResult { resultTiesToEven = -0.0
                , resultTiesToAway = -0.0
                , resultTowardPositive = -0.0
                , resultTowardNegative = -1.0
                , resultTowardZero = -0.0
                }
    RoundResult { resultTiesToEven = 0
                , resultTiesToAway = 0
                , resultTowardPositive = 0
                , resultTowardNegative = -1
                , resultTowardZero = 0
                }
  describe "-0.5" $ checkBehavior proxy (-0.5)
    RoundResult { resultTiesToEven = -0.0
                , resultTiesToAway = -1.0
                , resultTowardPositive = -0.0
                , resultTowardNegative = -1.0
                , resultTowardZero = -0.0
                }
    RoundResult { resultTiesToEven = 0
                , resultTiesToAway = -1
                , resultTowardPositive = 0
                , resultTowardNegative = -1
                , resultTowardZero = 0
                }
  describe "4.5" $ checkBehavior proxy 4.5
    RoundResult { resultTiesToEven = 4.0
                , resultTiesToAway = 5.0
                , resultTowardPositive = 5.0
                , resultTowardNegative = 4.0
                , resultTowardZero = 4.0
                }
    RoundResult { resultTiesToEven = 4
                , resultTiesToAway = 5
                , resultTowardPositive = 5
                , resultTowardNegative = 4
                , resultTowardZero = 4
                }
  describe "-5.5" $ checkBehavior proxy (-5.5)
    RoundResult { resultTiesToEven = -6.0
                , resultTiesToAway = -6.0
                , resultTowardPositive = -5.0
                , resultTowardNegative = -6.0
                , resultTowardZero = -5.0
                }
    RoundResult { resultTiesToEven = -6
                , resultTiesToAway = -6
                , resultTowardPositive = -5
                , resultTowardNegative = -6
                , resultTowardZero = -5
                }
  describe "-6.5" $ checkBehavior proxy (-6.5)
    RoundResult { resultTiesToEven = -6.0
                , resultTiesToAway = -7.0
                , resultTowardPositive = -6.0
                , resultTowardNegative = -7.0
                , resultTowardZero = -6.0
                }
    RoundResult { resultTiesToEven = -6
                , resultTiesToAway = -7
                , resultTowardPositive = -6
                , resultTowardNegative = -7
                , resultTowardZero = -6
                }

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "roundToIntegral" $ prop_roundToIntegral proxy
    checkCases proxy
  describe "Float" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "roundToIntegral" $ prop_roundToIntegral proxy
    checkCases proxy
