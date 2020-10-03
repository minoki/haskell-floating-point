module IntegerInternalsSpec (spec) where
import           Data.Int
import           Numeric.Floating.IEEE.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util
import Data.Bits
import Data.Maybe
import Math.NumberTheory.Logarithms

default ()

noinline :: a -> a
noinline = id
{-# NOINLINE noinline #-}

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  runIO $ putStrLn $ "The backend for IntegerInternals is " ++ integerInternalsBackend

  describe "integerToIntMaybe" $ do
    it "0" $ integerToIntMaybe 0 `shouldBe` Just 0
    it "123" $ integerToIntMaybe 123 `shouldBe` Just 123
    it "minBound :: Int" $ integerToIntMaybe (toInteger (minBound :: Int)) `shouldBe` Just minBound
    it "maxBound :: Int" $ integerToIntMaybe (toInteger (maxBound :: Int)) `shouldBe` Just maxBound
    it "(minBound :: Int) - 1" $ integerToIntMaybe (toInteger (minBound :: Int) - 1) `shouldBe` Nothing
    it "(maxBound :: Int) + 1" $ integerToIntMaybe (toInteger (maxBound :: Int) + 1) `shouldBe` Nothing
    prop "small integer" $ \x -> integerToIntMaybe (toInteger x) `shouldBe` Just x

  describe "integerToIntMaybe/noinline" $ do
    it "0" $ noinline integerToIntMaybe 0 `shouldBe` Just 0
    it "123" $ noinline integerToIntMaybe 123 `shouldBe` Just 123
    it "minBound :: Int" $ noinline integerToIntMaybe (toInteger (minBound :: Int)) `shouldBe` Just minBound
    it "maxBound :: Int" $ noinline integerToIntMaybe (toInteger (maxBound :: Int)) `shouldBe` Just maxBound
    it "(minBound :: Int) - 1" $ noinline integerToIntMaybe (toInteger (minBound :: Int) - 1) `shouldBe` Nothing
    it "(maxBound :: Int) + 1" $ noinline integerToIntMaybe (toInteger (maxBound :: Int) + 1) `shouldBe` Nothing
    prop "small integer" $ \x -> noinline integerToIntMaybe (toInteger x) `shouldBe` Just x

  prop "unsafeShiftLInteger" $ \x (NonNegative y) -> unsafeShiftLInteger x y `shouldBe` shiftL x y
  prop "unsafeShiftRInteger" $ \x (NonNegative y) -> unsafeShiftRInteger x y `shouldBe` shiftR x y

  describe "roundingMode" $ do
    prop "prop" $ \(Positive n) -> forAll (choose (0, integerLog2 n)) $ \e -> integerLog2 n >= e ==> roundingMode n e `shouldBe` compare (n `rem` 2^(e+1 :: Int)) (2^e)

  describe "countTrailingZerosInteger" $ do
    prop "test with Int64" $ \(NonZero x) -> countTrailingZerosInteger (fromIntegral x) == countTrailingZeros (x :: Int64)

  describe "integerIsPowerOf2" $ do
    prop "power of 2" $ \(NonNegative x) -> integerIsPowerOf2 (2^(x :: Int)) `shouldBe` Just x
    prop "(power of 2) + 1" $ \(Positive x) -> integerIsPowerOf2 (2^(x :: Int) + 1) `shouldBe` Nothing
    prop "(power of 2) - 1" $ \(Positive x) -> integerIsPowerOf2 (2^(x+1 :: Int) - 1) `shouldBe` Nothing

  prop "integerLog2IsPowerOf2" $ \(Positive x) -> integerLog2IsPowerOf2 x `shouldBe` (integerLog2 x, isJust (integerIsPowerOf2 x))
