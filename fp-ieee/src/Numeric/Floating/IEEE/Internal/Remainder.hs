{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Remainder
  ( remainder
  ) where
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Classify

default ()

-- |
-- IEEE 754 @remainder@ operation.
--
-- \(r=x-yn\), where n is the integer nearest the exact number \(n=\mathrm{round}(x/y)\)
remainder :: RealFloat a => a -> a -> a
remainder x y | isFinite x && isInfinite y = x
              | y == 0 || isInfinite y || isNaN y || not (isFinite x) = (x - x) / y * y -- return a NaN
              | otherwise = let n = round (toRational x / toRational y) -- TODO: Is round (x / y) okay?
                                r = fromRational (toRational x - toRational y * fromInteger n)
                            in r -- if r == 0, the sign of r is the same as x
{-# NOINLINE [1] remainder #-}

#if defined(USE_FFI)

foreign import ccall unsafe "remainderf"
  c_remainderFloat :: Float -> Float -> Float
foreign import ccall unsafe "remainder"
  c_remainderDouble :: Double -> Double -> Double

{-# RULES
"remainder/Float" remainder = c_remainderFloat
"remainder/Double" remainder = c_remainderDouble
  #-}

#endif
