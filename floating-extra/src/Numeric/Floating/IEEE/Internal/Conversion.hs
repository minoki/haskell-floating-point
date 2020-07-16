{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Conversion where
import MyPrelude

-- |
-- Similar to 'realToFrac', but treats NaN, infinities, negative zero even if the rewrite rule is off.
--
-- The properties of NaN may or may not be kept.
realFloatToFrac :: (RealFloat a, Fractional b) => a -> b
realFloatToFrac x | isNaN x = 0/0
                  | isInfinite x = if x > 0 then 1/0 else -1/0
                  | isNegativeZero x = -0
                  | otherwise = realToFrac x
{-# NOINLINE [1] realFloatToFrac #-}
{-# RULES
"realFloatToFrac/a->a" realFloatToFrac = id
"realFloatToFrac/Float->Double" realFloatToFrac = realToFrac :: Float -> Double -- should be rewritten into float2Double
"realFloatToFrac/Double->Float" realFloatToFrac = realToFrac :: Double -> Float -- should be rewritten into double2Float
  #-}
