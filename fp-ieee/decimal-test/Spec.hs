{-# LANGUAGE NumericUnderscores #-}
import qualified NextFloatSpec
import           Numeric.Decimal
import           Numeric.Floating.IEEE
import           Test.Hspec
import           Test.Hspec.Core.Spec

allowFailure :: String -> Item a -> Item a
allowFailure message item@(Item { itemExample = origExample }) = item { itemExample = newExample }
  where
    newExample params around callback = do
      result <- origExample params around callback
      case result of
        Result { resultStatus = Failure loc reason } -> do
          let message' = case reason of
                           NoReason -> message
                           _ -> message ++ ": " ++ show reason
          return result { resultStatus = Pending loc (Just message') }
        _ -> return result

main :: IO ()
main = hspec $ do
  mapSpecItem_ (allowFailure "decimal-arithmetic's floatRange may be incorrect") $ do
    it "maxFinite :: Decimal32" $ (maxFinite :: Decimal32) == 9.999_999e96 -- 7 digits
    it "maxFinite :: Decimal64" $ (maxFinite :: Decimal64) == 9.999_999_999_999_999e384 -- 16 digits
    it "maxFinite :: Decimal128" $ (maxFinite :: Decimal128) == 9.999_999_999_999_999_999_999_999_999_999_999e6144 -- 34 digits
  describe "NextFloat" NextFloatSpec.spec
