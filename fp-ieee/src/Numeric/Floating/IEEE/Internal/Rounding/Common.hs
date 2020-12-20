{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Rounding.Common where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Functor.Product
import           Data.Int
import           GHC.Float (expt)
import           Math.NumberTheory.Logarithms (integerLog2')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.IntegerInternals

default ()

class Functor f => RoundingStrategy f where
  exact :: a -> f a
  inexact :: Ordering -- ^ LT -> toward-zero is the nearest, EQ -> midpoint, GT -> away-from-zero is the nearest
          -> Bool -- ^ negative (True -> negative, False -> positive)
          -> Int -- ^ parity (even -> toward-zero is even, odd -> toward-zero is odd)
          -> a -- ^ toward zero
          -> a -- ^ away from zero
          -> f a
  doRound :: Bool -- ^ exactness; if True, the Ordering must be LT
          -> Ordering -- ^ LT -> toward-zero is the nearest, EQ -> midpoint, GT -> away-from-zero is the nearest
          -> Bool -- ^ negative (True -> negative, False -> positive)
          -> Int -- ^ parity (even -> toward-zero is even, odd -> toward-zero is odd)
          -> a -- ^ toward zero
          -> a -- ^ away from zero
          -> f a
  exact x = doRound True LT False 0 x x
  inexact o neg parity zero away = doRound False o neg parity zero away

newtype RoundTiesToEven a = RoundTiesToEven { roundTiesToEven :: a }
  deriving (Functor)

instance RoundingStrategy RoundTiesToEven where
  exact = RoundTiesToEven
  inexact o _neg parity zero away = RoundTiesToEven $ case o of
                                                        LT -> zero
                                                        EQ | even parity -> zero
                                                           | otherwise -> away
                                                        GT -> away
  doRound _ex o _neg parity zero away = RoundTiesToEven $ case o of
    LT -> zero
    EQ | even parity -> zero
       | otherwise -> away
    GT -> away
  {-# INLINE exact #-}
  {-# INLINE inexact #-}
  {-# INLINE doRound #-}

newtype RoundTiesToAway a = RoundTiesToAway { roundTiesToAway :: a }
  deriving (Functor)

instance RoundingStrategy RoundTiesToAway where
  exact = RoundTiesToAway
  inexact o _neg _parity zero away = RoundTiesToAway $ case o of
                                                         LT -> zero
                                                         EQ -> away
                                                         GT -> away
  doRound _ex o _neg _parity zero away = RoundTiesToAway $ case o of
    LT -> zero
    EQ -> away
    GT -> away
  {-# INLINE exact #-}
  {-# INLINE inexact #-}
  {-# INLINE doRound #-}

newtype RoundTowardPositive a = RoundTowardPositive { roundTowardPositive :: a }
  deriving (Functor)

instance RoundingStrategy RoundTowardPositive where
  exact = RoundTowardPositive
  inexact _o neg _parity zero away | neg = RoundTowardPositive zero
                                   | otherwise = RoundTowardPositive away
  doRound ex _o neg _parity zero away | ex || neg = RoundTowardPositive zero
                                      | otherwise = RoundTowardPositive away
  {-# INLINE exact #-}
  {-# INLINE inexact #-}
  {-# INLINE doRound #-}

newtype RoundTowardNegative a = RoundTowardNegative { roundTowardNegative :: a }
  deriving (Functor)

instance RoundingStrategy RoundTowardNegative where
  exact = RoundTowardNegative
  inexact _o neg _parity zero away | neg = RoundTowardNegative away
                                   | otherwise = RoundTowardNegative zero
  doRound ex _o neg _parity zero away | not ex && neg = RoundTowardNegative away
                                      | otherwise = RoundTowardNegative zero
  {-# INLINE exact #-}
  {-# INLINE inexact #-}
  {-# INLINE doRound #-}

newtype RoundTowardZero a = RoundTowardZero { roundTowardZero :: a }
  deriving (Functor)

instance RoundingStrategy RoundTowardZero where
  exact = RoundTowardZero
  inexact _o _neg _parity zero _away = RoundTowardZero zero
  doRound _ex _o _neg _parity zero _away = RoundTowardZero zero
  {-# INLINE exact #-}
  {-# INLINE inexact #-}
  {-# INLINE doRound #-}

instance (RoundingStrategy f, RoundingStrategy g) => RoundingStrategy (Product f g) where
  exact x = Pair (exact x) (exact x)
  inexact o neg parity zero away = Pair (inexact o neg parity zero away) (inexact o neg parity zero away)
  doRound ex o neg parity zero away = Pair (doRound ex o neg parity zero away) (doRound ex o neg parity zero away)
  {-# INLINE exact #-}
  {-# INLINE inexact #-}
  {-# INLINE doRound #-}

{-
from GHC.Float:
expt :: Integer -> Int -> Integer
expt base n = base ^ n
-}

quotRemByExpt :: Integer -- ^ the dividend @x@
              -> Integer -- ^ base
              -> Int -- ^ the exponent @e@ (must be non-negative)
              -> (Integer, Integer) -- ^ @x \`'quotRem'\` (base ^ e)@
quotRemByExpt x 2 n    = assert (n >= 0) (x `unsafeShiftRInteger` n, x .&. (bit n - 1))
quotRemByExpt x base n = x `quotRem` expt base n
{-# INLINE quotRemByExpt #-}

multiplyByExpt :: Integer -- ^ the multiplicand @x@
               -> Integer -- ^ base
               -> Int -- ^ the exponent @e@ (must be non-negative)
               -> Integer -- ^ @x * base ^ e@
multiplyByExpt x 2 n    = assert (n >= 0) (x `unsafeShiftLInteger` n)
multiplyByExpt x base n = x * expt base n
{-# INLINE multiplyByExpt #-}

isDivisibleByExpt :: Integer -- ^ the dividend @x@
                  -> Integer -- ^ the base
                  -> Int -- ^ the exponent @e@ (must be non-negative)
                  -> Integer -- ^ the remainder @r@ (must be @x \`'rem'\` (base ^ e)@)
                  -> Bool -- ^ @r == 0@
isDivisibleByExpt x 2 e r = assert (r == x `rem` (2 ^ e)) $ x == 0 || Numeric.Floating.IEEE.Internal.IntegerInternals.countTrailingZerosInteger x >= e
isDivisibleByExpt x base e r = assert (r == x `rem` (base ^ e)) (r == 0)
{-# INLINE isDivisibleByExpt #-}

-- |
-- Assumption: @n >= 0@, @e >= 0@, and @r == n \`'rem'\` base^(e+1)@
--
-- Returns @compare r (base^e)@.
compareWithExpt :: Integer -- ^ base
                -> Integer -- ^ the number @n@ (must be non-negative)
                -> Integer -- ^ the remainder @r@ (must be @n \`'rem'\' base^(e+1)@)
                -> Int -- ^ the exponent @e@ (must be non-negative)
                -> Ordering
compareWithExpt 2 n r e = assert (r == n `rem` expt 2 (e+1)) $
  if n == 0 || integerLog2' n < e then
    -- If integerLog2 n < e (i.e. n < 2^e), it is trivial
    LT
  else
    -- In this branch, n > 0 && integerLog2' n >= e
    let result = Numeric.Floating.IEEE.Internal.IntegerInternals.roundingMode n e
        !_ = assert (result == compare r (expt 2 e)) ()
    in result
compareWithExpt base n r e = assert (r == n `rem` expt base (e+1)) $ compare r (expt base e)
{-# INLINE compareWithExpt #-}
