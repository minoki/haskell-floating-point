import qualified AugmentedArithSpec
import qualified ClassificationSpec
import           Data.Proxy
import qualified FMASpec
import qualified NextAfterSpec
import qualified TwoSumSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Classification" ClassificationSpec.spec
  describe "TwoSum" TwoSumSpec.spec
  describe "FMA" FMASpec.spec
  describe "NextAfter" NextAfterSpec.spec
  describe "AugmentedArith" AugmentedArithSpec.spec
