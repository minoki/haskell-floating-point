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

-- |
-- IEEE 754 @minimumNumber@ operation.
-- @-0@ is smaller than @+0@.
-- Treats NaNs as missing data.
minimumNumber :: RealFloat a => a -> a -> a
minimumNumber x y | isNaN x && isNaN y = x + x
                  | x < y || isNaN y || (x == y && isNegativeZero x) = x
                  | otherwise = y

-- |
-- IEEE 754 @maximum@ operation.
-- @-0@ is smaller than @+0@.
-- Propagates NaNs.
maximum' :: RealFloat a => a -> a -> a
maximum' x y | isNaN x = x + x
             | isNaN y = y + y
             | x < y || (x == y && isNegativeZero x) = y
             | otherwise = x

-- |
-- IEEE 754 @maximumNumber@ operation.
-- @-0@ is smaller than @+0@.
-- Treats NaNs as missing data.
maximumNumber :: RealFloat a => a -> a -> a
maximumNumber x y | isNaN x && isNaN y = x + x
                  | x < y || isNaN x || (x == y && isNegativeZero x) = y
                  | otherwise = x

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
