{-
This module is the custom Prelude for this project.
You can replace Prelude's definition by a debugging-friendly one.
Examples are:

type RealFloat a = (Prelude.RealFloat a, Show a)

(^) :: (HasCallStack, Num a, Integral b) => a -> b -> a
x ^ y | y < 0 = error "Negative exponent" -- with stack trace
      | otherwise = x Prelude.^ y
-}

module MyPrelude (module Prelude) where
import Prelude
