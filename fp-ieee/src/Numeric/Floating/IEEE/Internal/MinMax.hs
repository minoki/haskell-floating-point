{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.MinMax where
import           MyPrelude

default ()

-- |
-- IEEE 754 @minimum@ operation.
-- @-0@ is smaller than @+0@.
-- Propagates NaNs.
minimum' :: RealFloat a => a -> a -> a
minimum' x y | isNaN x = x + x
             | isNaN y = y + y
             | x < y || (x == y && isNegativeZero x) = x
             | otherwise = y
{-# NOINLINE [1] minimum' #-}

-- |
-- IEEE 754 @minimumNumber@ operation.
-- @-0@ is smaller than @+0@.
-- Treats NaNs as missing data.
minimumNumber :: RealFloat a => a -> a -> a
minimumNumber x y | isNaN x && isNaN y = x + x
                  | x < y || isNaN y || (x == y && isNegativeZero x) = x
                  | otherwise = y
{-# NOINLINE [1] minimumNumber #-}

-- |
-- IEEE 754 @maximum@ operation.
-- @-0@ is smaller than @+0@.
-- Propagates NaNs.
maximum' :: RealFloat a => a -> a -> a
maximum' x y | isNaN x = x + x
             | isNaN y = y + y
             | x < y || (x == y && isNegativeZero x) = y
             | otherwise = x
{-# NOINLINE [1] maximum' #-}

-- |
-- IEEE 754 @maximumNumber@ operation.
-- @-0@ is smaller than @+0@.
-- Treats NaNs as missing data.
maximumNumber :: RealFloat a => a -> a -> a
maximumNumber x y | isNaN x && isNaN y = x + x
                  | x < y || isNaN x || (x == y && isNegativeZero x) = y
                  | otherwise = x
{-# NOINLINE [1] maximumNumber #-}

-- |
-- IEEE 754 @minimumMagnitude@ operation.
minimumMagnitude :: RealFloat a => a -> a -> a
minimumMagnitude x y | abs x < abs y = x
                     | abs y < abs x = y
                     | otherwise = minimum' x y

-- |
-- IEEE 754 @minimumMagnitudeNumber@ operation.
minimumMagnitudeNumber :: RealFloat a => a -> a -> a
minimumMagnitudeNumber x y | abs x < abs y = x
                           | abs y < abs x = y
                           | otherwise = minimumNumber x y

-- |
-- IEEE 754 @maximumMagnitude@ operation.
maximumMagnitude :: RealFloat a => a -> a -> a
maximumMagnitude x y | abs x > abs y = x
                     | abs y > abs x = y
                     | otherwise = maximum' x y

-- |
-- IEEE 754 @maximumMagnitudeNumber@ operation.
maximumMagnitudeNumber :: RealFloat a => a -> a -> a
maximumMagnitudeNumber x y | abs x > abs y = x
                           | abs y > abs x = y
                           | otherwise = maximumNumber x y

#if defined(HAS_FAST_MINMAX)

foreign import ccall unsafe "hs_minimumFloat"
  minimumFloat :: Float -> Float -> Float
foreign import ccall unsafe "hs_maximumFloat"
  maximumFloat :: Float -> Float -> Float
foreign import ccall unsafe "hs_minimumNumberFloat"
  minimumNumberFloat :: Float -> Float -> Float
foreign import ccall unsafe "hs_maximumNumberFloat"
  maximumNumberFloat :: Float -> Float -> Float
foreign import ccall unsafe "hs_minimumDouble"
  minimumDouble :: Double -> Double -> Double
foreign import ccall unsafe "hs_maximumDouble"
  maximumDouble :: Double -> Double -> Double
foreign import ccall unsafe "hs_minimumNumberDouble"
  minimumNumberDouble :: Double -> Double -> Double
foreign import ccall unsafe "hs_maximumNumberDouble"
  maximumNumberDouble :: Double -> Double -> Double

{-# RULES
"minimum'/Float" minimum' = minimumFloat
"maximum'/Float" maximum' = maximumFloat
"minimumNumber/Float" minimumNumber = minimumNumberFloat
"maximumNumber/Float" maximumNumber = maximumNumberFloat
"minimum'/Double" minimum' = minimumDouble
"maximum'/Double" maximum' = maximumDouble
"minimumNumber/Double" minimumNumber = minimumNumberDouble
"maximumNumber/Double" maximumNumber = maximumNumberDouble
  #-}

#else

minimumFloat :: Float -> Float -> Float
maximumFloat :: Float -> Float -> Float
minimumNumberFloat :: Float -> Float -> Float
maximumNumberFloat :: Float -> Float -> Float
minimumDouble :: Double -> Double -> Double
maximumDouble :: Double -> Double -> Double
minimumNumberDouble :: Double -> Double -> Double
maximumNumberDouble :: Double -> Double -> Double

minimumFloat = minimum'
minimumDouble = minimum'
minimumNumberFloat = minimumNumber
minimumNumberDouble = minimumNumber
maximumFloat = maximum'
maximumDouble = maximum'
maximumNumberFloat = maximumNumber
maximumNumberDouble = maximumNumber

#endif
