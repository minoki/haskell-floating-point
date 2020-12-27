{-|
Module      : Numeric.Floating.IEEE.NaN
Description : Accessing the sign and payload of NaNs

This module provides the typeclass for NaN manipulation: 'RealFloatNaN'.

In addition to 'Float' and 'Double', a couple of floating-point types provided by third-party libraries can be supported via package flags: @Half@ via @half@ and @Float128@ via @float128@.
-}
module Numeric.Floating.IEEE.NaN
  ( RealFloatNaN(..)
  , Class(..)
  , TotallyOrdered(..)
  ) where
import           Numeric.Floating.IEEE.Internal ()
import           Numeric.Floating.IEEE.Internal.NaN
