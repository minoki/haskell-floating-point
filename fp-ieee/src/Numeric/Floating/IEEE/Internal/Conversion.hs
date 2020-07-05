{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Conversion where
import           GHC.Float.Compat (double2Float, float2Double)
import           MyPrelude

default ()

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
"realFloatToFrac/a->a" forall x. realFloatToFrac x = x * 1.0 -- trying to make signaling NaNs quiet. Unfortunately, GHC optimizes away '* 1.0' when the type is 'Float' or 'Double'.
"realFloatToFrac/Float->Double" realFloatToFrac = float2Double
"realFloatToFrac/Double->Float" realFloatToFrac = double2Float
  #-}
