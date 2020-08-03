{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

#include "MachDeps.h"

module Numeric.Floating.IEEE.Internal.IntegerInternals
  ( integerToIntMaybe
  , naturalToWordMaybe
  , unsafeShiftLInteger
  , unsafeShiftRInteger
  , roundingMode
  , countTrailingZerosInteger
  ) where
import           Data.Bits
import           GHC.Exts
import           GHC.Int (Int (I#))
import           GHC.Word (Word (W#))
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Natural
#if defined(MIN_VERSION_ghc_bignum)
import           GHC.Num.BigNat
import           GHC.Num.Integer (Integer (IN, IP, IS))
import qualified GHC.Num.Integer
import           GHC.Num.Natural (Natural (NS))
#elif defined(MIN_VERSION_integer_gmp)
import qualified GHC.Integer
import           GHC.Integer.GMP.Internals (Integer (Jn#, Jp#, S#),
                                            indexBigNat#)
import qualified GHC.Integer.Logarithms.Internals
import           GHC.Natural (Natural (NatS#))
#endif

integerToIntMaybe :: Integer -> Maybe Int
naturalToWordMaybe :: Natural -> Maybe Word

-- The instance 'Bits Integer' is not very optimized...
unsafeShiftLInteger :: Integer -> Int -> Integer
unsafeShiftRInteger :: Integer -> Int -> Integer

roundingMode :: Integer -- ^ must be positive
             -> Int -- ^ must be positive
             -> Ordering

-- |
-- 'Integer' version of 'countTrailingZeros'.
-- The argument must not be zero.
countTrailingZerosInteger :: Integer -> Int

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
                            (# s, r #) -> let !w = bigNatIndex# bn s
                                              -- w `shiftL` (WORD_SIZE_IN_BITS - r - 1) vs. 1 `shiftL` (WORD_SIZE_IN_BITS - 1)
                                          in compare (W# (w `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1# -# r))) (W# (1## `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1#)))
                                             <> loop s
  where
    loop i = case bigNatIndex# bn i of
               0## -> case i of
                        0# -> EQ
                        _  -> loop (i -# 1#)
               _ -> LT

roundingMode x (I# t) = roundingMode# x t
{-# INLINE roundingMode #-}

{-
isDivisibleByPowerOf2# :: Integer -> Int# -> Bool
isDivisibleByPowerOf2# (IS 0#) t = True
isDivisibleByPowerOf2# (IS x) t = isTrue# (t <# WORD_SIZE_IN_BITS#)
                                  && case int2Word# x `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1# -# t) of
                                       0## -> True
                                       _ -> False
isDivisibleByPowerOf2# (IN bn) t = isDivisibleByPowerOf2 (IP bn) t
isDivisibleByPowerOf2# (IP bn) t = case t `quotRemInt#` WORD_SIZE_IN_BITS# of
                                     (# s, r #) -> isTrue# (s <# bigNatSize# bn)
                                                   && case bigNatIndex# bn s `uncheckedShiftL#` (WORD_SIZE_IN_BITS# -# 1# -# r) of
                                                        0## -> loop s
                                                        _ -> False
  where loop i = case bigNatIndex# bn i of
                   0## -> case i of
                            0# -> True
                            _ -> loop (i -# 1#)
                   _ -> False

isDivisibleByPowerOf2 x (I# t) = isDivisibleByPowerOf2# x t
{-# INLINE isDivisibleByPowerOf2 #-}
-}

countTrailingZerosInteger# :: Integer -> Word#
countTrailingZerosInteger# (IS x) = ctz# (int2Word# x)
countTrailingZerosInteger# (IN bn) = countTrailingZerosInteger# (IP bn)
countTrailingZerosInteger# (IP bn) = loop 0# 0##
  where
    loop i acc = case bigNatIndex# bn i of -- `i < bigNatSize# bn` must hold
                   0## -> loop (i +# 1#) (acc `plusWord#` WORD_SIZE_IN_BITS##)
                   w -> acc `plusWord#` ctz# w

countTrailingZerosInteger 0 = error "countTrailingZerosInteger: zero"
countTrailingZerosInteger x = I# (word2Int# (countTrailingZerosInteger# x))
{-# INLINE countTrailingZerosInteger #-}

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

#else

roundingMode x t = compare (x .&. (bit (t + 1) - 1)) (bit t)
{-# INLINE roundingMode #-}

countTrailingZerosInteger 0 = error "countTrailingZerosInteger: zero"
countTrailingZerosInteger x = integerLog2' (x `xor` (x - 1))
{-# INLINE countTrailingZerosInteger #-}

#endif

-- TODO: integerIsPowerOf2
