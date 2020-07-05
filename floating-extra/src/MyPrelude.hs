{-# LANGUAGE ConstraintKinds #-}
module MyPrelude (module Prelude, module MyPrelude) where
import Prelude hiding (RealFloat)
import qualified Prelude

type RealFloat a = (Prelude.RealFloat a, Show a)
