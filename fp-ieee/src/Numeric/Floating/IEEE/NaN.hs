module Numeric.Floating.IEEE.NaN
  ( SupportsNaN(..)
  , classify
  , Class (..)
  , TotallyOrdered(..)
  , compareByTotalOrder
  ) where
import           Numeric.Floating.IEEE.Internal () -- orphan instances
import           Numeric.Floating.IEEE.Internal.NaN
