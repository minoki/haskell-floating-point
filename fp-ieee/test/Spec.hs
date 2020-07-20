import qualified AugmentedArithSpec
import qualified ClassificationSpec
import qualified FMASpec
import qualified NaNSpec
import qualified NextFloatSpec
import qualified RoundingSpec
import           System.Environment (getArgs, withArgs)
import           Test.Hspec hiding (hspec)
import           Test.Hspec.Core.Runner hiding (hspec)
import qualified TwoSumSpec

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
  describe "NextFloat" NextFloatSpec.spec
  describe "AugmentedArith" AugmentedArithSpec.spec
  describe "Rounding" RoundingSpec.spec
  describe "NaN" NaNSpec.spec
