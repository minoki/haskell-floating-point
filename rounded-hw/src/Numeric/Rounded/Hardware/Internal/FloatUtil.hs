{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
module Numeric.Rounded.Hardware.Internal.FloatUtil
  ( nextUp
  , nextDown
  , nextTowardZero
  , distanceUlp
  , fusedMultiplyAdd
  ) where
import           Data.Ratio
import           Numeric.Floating.IEEE

distanceUlp :: RealFloat a => a -> a -> Maybe Integer
distanceUlp x y
  | isInfinite x || isInfinite y || isNaN x || isNaN y = Nothing
  | otherwise = let m = min (abs x) (abs y)
                    m' = nextUp m
                    v = (toRational y - toRational x) / toRational (m' - m)
                in if denominator v == 1
                   then Just (abs (numerator v))
                   else error "distanceUlp"
