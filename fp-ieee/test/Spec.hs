{-# LANGUAGE CPP #-}
import qualified AugmentedArithSpec
import qualified ClassificationSpec
import qualified FMASpec
import qualified IntegerInternalsSpec
import qualified MinMaxSpec
import qualified NaNSpec
import qualified NextFloatSpec
import qualified RoundingSpec
import qualified RoundToIntegralSpec
import           System.Environment (getArgs, withArgs)
import           Test.Hspec hiding (hspec)
import           Test.Hspec.Core.Runner hiding (hspec)
import qualified TwoSumSpec
#if defined(USE_HALF)
import qualified HalfSpec
#endif
#if defined(USE_FLOAT128)
import qualified Float128Spec
#endif

-- "Extra" tests are not run by default; set --skip "***" to run them.
myFilter :: Path -> Bool
myFilter (groups, _description) = "Extra" `elem` groups

withDefaultFilter :: Config -> Config
withDefaultFilter config@(Config { configSkipPredicate = Nothing }) = config { configSkipPredicate = Just myFilter }
withDefaultFilter config = config

hspec :: Spec -> IO ()
hspec spec =
  getArgs
  >>= readConfig defaultConfig
  >>= withArgs [] . runSpec spec . withDefaultFilter
  >>= evaluateSummary

main :: IO ()
main = hspec $ do
  describe "Classification" ClassificationSpec.spec
  describe "TwoSum" TwoSumSpec.spec
  describe "FMA" FMASpec.spec
  describe "IntegerInternals" IntegerInternalsSpec.spec
  describe "NextFloat" NextFloatSpec.spec
  describe "AugmentedArith" AugmentedArithSpec.spec
  describe "Rounding" RoundingSpec.spec
  describe "RoundToIntegral" RoundToIntegralSpec.spec
  describe "NaN" NaNSpec.spec
  describe "MinMax" MinMaxSpec.spec
#if defined(USE_HALF)
  describe "Half" HalfSpec.spec
#endif
#if defined(USE_FLOAT128)
  describe "Float128" Float128Spec.spec
#endif
