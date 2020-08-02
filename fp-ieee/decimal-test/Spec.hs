import qualified NextFloatSpec
import           Numeric.Decimal
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "NextFloat" NextFloatSpec.spec
