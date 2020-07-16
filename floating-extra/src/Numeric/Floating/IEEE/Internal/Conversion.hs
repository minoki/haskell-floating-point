{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Conversion where
import           MyPrelude
#if defined(USE_HALF) && defined(HAS_FAST_HALF_CONVERSION)
import           Data.Coerce
import           Data.Word
import           Foreign.C.Types
import           Numeric.Half
#endif

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

#if defined(HAS_FAST_HALF_CONVERSION)

foreign import ccall unsafe "hs_fastHalfToFloat"
  c_fastHalfToFloat :: Word16 -> Float
foreign import ccall unsafe "hs_fastHalfToDouble"
  c_fastHalfToDouble :: Word16 -> Double
foreign import ccall unsafe "hs_fastFloatToHalf"
  c_fastFloatToHalf :: Float -> Word16
foreign import ccall unsafe "hs_fastDoubleToHalf"
  c_fastDoubleToHalf :: Double -> Word16

fastHalfToFloat :: Half -> Float
fastHalfToFloat = coerce c_fastHalfToFloat
{-# INLINE fastHalfToFloat #-}

fastHalfToDouble :: Half -> Double
fastHalfToDouble = coerce c_fastHalfToDouble
{-# INLINE fastHalfToDouble #-}

fastFloatToHalf :: Float -> Half
fastFloatToHalf = coerce c_fastFloatToHalf
{-# INLINE fastFloatToHalf #-}

fastDoubleToHalf :: Double -> Half
fastDoubleToHalf = coerce c_fastDoubleToHalf
{-# INLINE fastDoubleToHalf #-}

#endif
