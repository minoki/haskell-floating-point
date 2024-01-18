import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "-fobject-code" -- for GHC 8.6
               , "src/Numeric/Floating/IEEE/Internal/Augmented.hs"
               , "src/Numeric/Floating/IEEE/Internal/Base.hs"
               , "src/Numeric/Floating/IEEE/Internal/Classify.hs"
               , "src/Numeric/Floating/IEEE/Internal/FMA.hs"
               , "src/Numeric/Floating/IEEE/Internal/GenericArith.hs"
               , "src/Numeric/Floating/IEEE/Internal/IntegerInternals.hs"
               , "src/Numeric/Floating/IEEE/Internal/MinMax.hs"
               , "src/Numeric/Floating/IEEE/Internal/NextFloat.hs"
               , "src/Numeric/Floating/IEEE/Internal/Rounding/Common.hs"
               , "src/Numeric/Floating/IEEE/Internal/Rounding/Rational.hs"
               , "src/Numeric/Floating/IEEE/Internal/RoundToIntegral.hs"
               ]
