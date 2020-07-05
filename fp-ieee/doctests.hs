import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Numeric/Floating/IEEE/Internal/Base.hs"
               , "src/Numeric/Floating/IEEE/Internal/Classify.hs"
               , "src/Numeric/Floating/IEEE/Internal/FMA.hs"
               , "src/Numeric/Floating/IEEE/Internal/GenericArith.hs"
               , "src/Numeric/Floating/IEEE/Internal/MinMax.hs"
               , "src/Numeric/Floating/IEEE/Internal/NextFloat.hs"
               , "src/Numeric/Floating/IEEE/Internal/RoundToIntegral.hs"
               ]
