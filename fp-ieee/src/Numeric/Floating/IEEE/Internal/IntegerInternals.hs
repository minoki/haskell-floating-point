{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unused-imports -fobject-code #-}

#include "MachDeps.h"

module Numeric.Floating.IEEE.Internal.IntegerInternals
  ( integerToIntMaybe
  , naturalToWordMaybe
  , unsafeShiftLInteger
  , unsafeShiftRInteger
  , roundingMode
  , countTrailingZerosInteger
  , integerIsPowerOf2
  , integerLog2IsPowerOf2
  ) where
import           Data.Bits
import           GHC.Exts (Int#, Word#, ctz#, int2Word#, plusWord#, quotRemInt#,
                           uncheckedShiftL#, word2Int#, (+#), (-#))
import           GHC.Int (Int (I#))
import           GHC.Word (Word (W#))
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Natural
#if defined(MIN_VERSION_ghc_bignum)
import qualified GHC.Num.BigNat
import           GHC.Num.Integer (Integer (IN, IP, IS))
import qualified GHC.Num.Integer
import           GHC.Num.Natural (Natural (NS))
#elif defined(MIN_VERSION_integer_gmp)
import qualified GHC.Integer
import           GHC.Integer.GMP.Internals (Integer (Jn#, Jp#, S#),
                                            indexBigNat#)
import qualified GHC.Integer.Logarithms.Internals
import           GHC.Natural (Natural (NatS#))
#else
import           Math.NumberTheory.Logarithms (integerLog2')
#endif

-- $setup
-- >>> :m + Data.Int Test.QuickCheck
-- >>> :{
--   -- Workaround for https://github.com/sol/doctest/issues/160:
--   import Numeric.Floating.IEEE.Internal.IntegerInternals
-- :}

integerToIntMaybe :: Integer -> Maybe Int
naturalToWordMaybe :: Natural -> Maybe Word

-- The instance 'Bits Integer' is not very optimized...
unsafeShiftLInteger :: Integer -> Int -> Integer
unsafeShiftRInteger :: Integer -> Int -> Integer

-- |
-- Assumption: @n > 0@, @e >= 0@, and @integerLog2 n >= e@
--
-- Returns @compare (n \`'rem'\` 2^(e+1)) (2^e)@.
roundingMode :: Integer -- ^ @n@
             -> Int -- ^ @e@
             -> Ordering

-- |
-- 'Integer' version of 'countTrailingZeros'.
-- The argument must not be zero.
--
-- prop> \(NonZero x) -> countTrailingZerosInteger (toInteger x) === countTrailingZeros (x :: Int64)
-- >>> countTrailingZerosInteger 7
-- 0
-- >>> countTrailingZerosInteger 8
-- 3
countTrailingZerosInteger :: Integer -> Int

-- |
-- Returns @Just (integerLog2 x)@ if the argument @x@ is a power of 2, and @Nothing@ otherwise.
-- The argument @x@ must be strictly positive.
integerIsPowerOf2 :: Integer -> Maybe Int

-- |
-- Returns @(integerLog2 x, isJust (integerIsPowerOf2 x))@.
-- The argument @x@ must be strictly positive.
integerLog2IsPowerOf2 :: Integer -> (Int, Bool)

#if defined(MIN_VERSION_ghc_bignum)

integerToIntMaybe (IS x) = Just (I# x)
integerToIntMaybe _      = Nothing -- relies on Integer's invariant

naturalToWordMaybe (NS x) = Just (W# x)
naturalToWordMaybe _      = Nothing -- relies on Natural's invariant

unsafeShiftLInteger x (I# i) = GHC.Num.Integer.integerShiftL# x (int2Word# i)
unsafeShiftRInteger x (I# i) = GHC.Num.Integer.integerShiftR# x (int2Word# i)

#elif defined(MIN_VERSION_integer_gmp)

integerToIntMaybe (S# x) = Just (I# x)
integerToIntMaybe _      = Nothing -- relies on Integer's invariant

naturalToWordMaybe (NatS# x) = Just (W# x)
naturalToWordMaybe _         = Nothing -- relies on Natural's invariant

unsafeShiftLInteger x (I# i) = GHC.Integer.shiftLInteger x i
unsafeShiftRInteger x (I# i) = GHC.Integer.shiftRInteger x i

#else

integerToIntMaybe = toIntegralSized
naturalToWordMaybe = toIntegralSized

unsafeShiftLInteger = unsafeShiftL
unsafeShiftRInteger = unsafeShiftR

#endif

{-# INLINE [0] integerToIntMaybe #-}
{-# INLINE [0] naturalToWordMaybe #-}

-- Allow constant folding of integerToIntMaybe and naturalToWordMaybe
-- NB: https://gitlab.haskell.org/ghc/ghc/-/issues/18526 needs to be worked around.

minBoundIntAsInteger :: Integer
minBoundIntAsInteger = fromIntegral (minBound :: Int)
{-# INLINE minBoundIntAsInteger #-}

maxBoundIntAsInteger :: Integer
maxBoundIntAsInteger = fromIntegral (maxBound :: Int)
{-# INLINE maxBoundIntAsInteger #-}

maxBoundWordAsNatural :: Natural
maxBoundWordAsNatural = fromIntegral (maxBound :: Word)
{-# INLINE maxBoundWordAsNatural #-}

integerToIntMaybe2 :: Bool -> Integer -> Maybe Int
integerToIntMaybe2 _ = integerToIntMaybe
{-# INLINE [0] integerToIntMaybe2 #-}

naturalToWordMaybe2 :: Bool -> Natural -> Maybe Word
naturalToWordMaybe2 _ = naturalToWordMaybe
{-# INLINE [0] naturalToWordMaybe2 #-}

{-# RULES
"integerToIntMaybe" [~0] forall x.
  integerToIntMaybe x = integerToIntMaybe2 (minBoundIntAsInteger <= x && x <= maxBoundIntAsInteger) x
"integerToIntMaybe2/small" forall x.
  integerToIntMaybe2 True x = Just (fromIntegral x)
"integerToIntMaybe2/large" forall x.
  integerToIntMaybe2 False x = Nothing
"naturalToWordMaybe" [~0] forall x.
  naturalToWordMaybe x = naturalToWordMaybe2 (x <= maxBoundWordAsNatural) x
"naturalToWordIntMaybe2/small" forall x.
  naturalToWordMaybe2 True x = Just (fromIntegral x)
"naturalToWordIntMaybe2/large" forall x.
  naturalToWordMaybe2 False x = Nothing
  #-}

{-# INLINE unsafeShiftLInteger #-}
{-# INLINE unsafeShiftRInteger #-}

#if defined(MIN_VERSION_ghc_bignum)

roundingMode# :: Integer -> Int# -> Ordering
roundingMode# (IS x) t = let !w = int2Word# x
                         in compare (W# (w `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1# -# t))) (W# (1## `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1#)))
roundingMode# (IN bn) t = roundingMode# (IP bn) t -- unexpected
roundingMode# (IP bn) t = case t `quotRemInt#` WORD_SIZE_IN_BITS# of
                            -- 0 <= r < WORD_SIZE_IN_BITS
                            (# s, r #) -> let !w = GHC.Num.BigNat.bigNatIndex# bn s
                                              -- w `shiftL` (WORD_SIZE_IN_BITS - r - 1) vs. 1 `shiftL` (WORD_SIZE_IN_BITS - 1)
                                          in compare (W# (w `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1# -# r))) (W# (1## `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1#)))
                                             <> loop s
  where
    loop i = case GHC.Num.BigNat.bigNatIndex# bn i of
               0## -> case i of
                        0# -> EQ
                        _  -> loop (i -# 1#)
               _ -> LT

roundingMode x (I# t) = roundingMode# x t
{-# INLINE roundingMode #-}

countTrailingZerosInteger# :: Integer -> Word#
countTrailingZerosInteger# (IS x) = ctz# (int2Word# x)
countTrailingZerosInteger# (IN bn) = countTrailingZerosInteger# (IP bn)
countTrailingZerosInteger# (IP bn) = loop 0# 0##
  where
    loop i acc = case GHC.Num.BigNat.bigNatIndex# bn i of -- `i < bigNatSize# bn` must hold
                   0## -> loop (i +# 1#) (acc `plusWord#` WORD_SIZE_IN_BITS##)
                   w -> acc `plusWord#` ctz# w

countTrailingZerosInteger 0 = error "countTrailingZerosInteger: zero"
countTrailingZerosInteger x = I# (word2Int# (countTrailingZerosInteger# x))
{-# INLINE countTrailingZerosInteger #-}

integerIsPowerOf2 x = case GHC.Num.Integer.integerIsPowerOf2# x of
                        (# _ | #) -> Nothing
                        (# | w #) -> Just (I# (word2Int# w))
{-# INLINE integerIsPowerOf2 #-}

integerLog2IsPowerOf2 x = case GHC.Num.Integer.integerIsPowerOf2# x of
                            (# _ | #) -> (I# (word2Int# (GHC.Num.Integer.integerLog2# x)), False)
                            (# | w #) -> (I# (word2Int# w), True)
{-# INLINE integerLog2IsPowerOf2 #-}

#elif defined(MIN_VERSION_integer_gmp)

roundingMode x (I# t#) = case GHC.Integer.Logarithms.Internals.roundingMode# x t# of
                           0# -> LT -- round toward zero
                           1# -> EQ -- half
                           _  -> GT -- 2#: round away from zero
{-# INLINE roundingMode #-}

countTrailingZerosInteger# :: Integer -> Word#
countTrailingZerosInteger# (S# x) = ctz# (int2Word# x)
countTrailingZerosInteger# (Jn# bn) = countTrailingZerosInteger# (Jp# bn)
countTrailingZerosInteger# (Jp# bn) = loop 0# 0##
  where
    loop i acc = case indexBigNat# bn i of -- `i < sizeOfBigNat# bn` must hold
                   0## -> loop (i +# 1#) (acc `plusWord#` WORD_SIZE_IN_BITS##)
                   w -> acc `plusWord#` ctz# w

countTrailingZerosInteger 0 = error "countTrailingZerosInteger: zero"
countTrailingZerosInteger x = I# (word2Int# (countTrailingZerosInteger# x))
{-# INLINE countTrailingZerosInteger #-}

integerIsPowerOf2 x = case GHC.Integer.Logarithms.Internals.integerLog2IsPowerOf2# x of
                        (# l, 0# #) -> Just (I# l)
                        (# _, _ #)  -> Nothing
{-# INLINE integerIsPowerOf2 #-}

integerLog2IsPowerOf2 x = case GHC.Integer.Logarithms.Internals.integerLog2IsPowerOf2# x of
                        (# l, 0# #) -> (I# l, True)
                        (# l, _ #)  -> (I# l, False)
{-# INLINE integerLog2IsPowerOf2 #-}

#else

roundingMode x t = compare (x .&. (bit (t + 1) - 1)) (bit t)
{-# INLINE roundingMode #-}

countTrailingZerosInteger 0 = error "countTrailingZerosInteger: zero"
countTrailingZerosInteger x = integerLog2' (x `xor` (x - 1))
{-# INLINE countTrailingZerosInteger #-}

integerIsPowerOf2 x = x .&. (x - 1) == 0

integerLog2IsPowerOf2 x = (integerLog2' x, integerIsPowerOf2 x)
{-# INLINE integerLog2IsPowerOf2 #-}

#endif
