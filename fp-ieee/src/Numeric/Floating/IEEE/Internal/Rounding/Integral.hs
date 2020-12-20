{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module Numeric.Floating.IEEE.Internal.Rounding.Integral where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Functor.Product
import           Data.Int
import           Data.Proxy
import           Data.Word
import           GHC.Exts
import           Math.NumberTheory.Logarithms (integerLog2', integerLogBase',
                                               wordLog2')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
import           Numeric.Floating.IEEE.Internal.IntegerInternals
import           Numeric.Floating.IEEE.Internal.Rounding.Common

default ()

fromIntegerR :: (RealFloat a, RoundingStrategy f) => Integer -> f a
fromIntegerR n = case integerToIntMaybe n of
                   Just x -> fromIntegralRBits x
                   Nothing | n < 0 -> negate <$> fromPositiveIntegerR True (- n)
                           | otherwise -> fromPositiveIntegerR False n
{-# INLINE fromIntegerR #-}

fromIntegralR :: (Integral i, RealFloat a, RoundingStrategy f) => i -> f a
fromIntegralR x = fromIntegerR (toInteger x)
{-# INLINE [0] fromIntegralR #-}
{-# RULES
"fromIntegralR/Integer->a" fromIntegralR = fromIntegerR
"fromIntegralR/Int->a" fromIntegralR = fromIntegralRBits @Int
"fromIntegralR/Int8->a" fromIntegralR = fromIntegralRBits @Int8
"fromIntegralR/Int16->a" fromIntegralR = fromIntegralRBits @Int16
"fromIntegralR/Int32->a" fromIntegralR = fromIntegralRBits @Int32
"fromIntegralR/Int64->a" fromIntegralR = fromIntegralRBits @Int64
"fromIntegralR/Word->a" fromIntegralR = fromIntegralRBits @Word
"fromIntegralR/Word8->a" fromIntegralR = fromIntegralRBits @Word8
"fromIntegralR/Word16->a" fromIntegralR = fromIntegralRBits @Word16
"fromIntegralR/Word32->a" fromIntegralR = fromIntegralRBits @Word32
"fromIntegralR/Word64->a" fromIntegralR = fromIntegralRBits @Word64
  #-}

fromIntegralRBits :: (Integral i, Bits i, RealFloat a, RoundingStrategy f) => i -> f a
fromIntegralRBits x
  -- Small enough: fromIntegral should be sufficient
  | ieee
  , let resultI = fromIntegral x
  , let (min', max') = boundsForExactConversion (resultI <$ Proxy)
  , maybe True (<= x) min'
  , maybe True (x <=) max'
  = exact resultI

  -- Signed, and not small enough: Test if the value fits in Int
  | ieee
  , base == 2
  , signed
  , Just y <- toIntegralSized x :: Maybe Int
  = if y < 0 then
      negate <$> positiveWordToBinaryFloatR True (negateIntAsWord y)
    else
      -- We can assume x /= 0
      positiveWordToBinaryFloatR False (fromIntegral y)

  -- Unsigned, and not small enough: Test if the value fits in Word
  | ieee
  , base == 2
  , not signed
  , Just y <- toIntegralSized x :: Maybe Word
  = -- We can assume x /= 0
    positiveWordToBinaryFloatR False y

  -- General case: Convert via Integer
  | otherwise = result
  where
    result | x == 0 = exact 0
           | x < 0 = negate <$> fromPositiveIntegerR True (- toInteger x)
           | otherwise = fromPositiveIntegerR False (toInteger x)
    signed = isSigned x
    ieee = isIEEE (undefined `asProxyTypeOf` result)
    base = floatRadix (undefined `asProxyTypeOf` result)
{-# INLINE fromIntegralRBits #-}

-- |
-- >>> boundsForExactConversion (Proxy :: Proxy Double) :: (Maybe Integer, Maybe Integer) -- (Just (-2^53),Just (2^53))
-- (Just (-9007199254740992),Just 9007199254740992)
-- >>> boundsForExactConversion (Proxy :: Proxy Double) :: (Maybe Int32, Maybe Int32) -- the conversion is always exact
-- (Nothing,Nothing)
-- >>> boundsForExactConversion (Proxy :: Proxy Float) :: (Maybe Word, Maybe Word) -- (Nothing,Just (2^24))
-- (Nothing,Just 16777216)
boundsForExactConversion :: (Integral i, Bits i, RealFloat a) => Proxy a -> (Maybe i, Maybe i)
boundsForExactConversion proxyR = assert ieee (minI, maxI)
  where
    maxInteger = base ^! digits
    minInteger = - maxInteger
    minI = case minBoundAsInteger (undefined `asProxyTypeOf` minI) of
             Just minBound' | minInteger <= minBound' -> Nothing -- all negative integers can be expressed in the target floating-type: no check for lower-bound is needed
             _ -> Just (fromInteger minInteger)
    maxI = case maxBoundAsInteger (undefined `asProxyTypeOf` maxI) of
             Just maxBound' | maxBound' <= maxInteger -> Nothing -- all positive integral values can be expressed in the target floating-type: no check for upper-bound is needed
             _ -> Just (fromInteger maxInteger)
    ieee = isIEEE (undefined `asProxyTypeOf` proxyR)
    base = floatRadix (undefined `asProxyTypeOf` proxyR)
    digits = floatDigits (undefined `asProxyTypeOf` proxyR)
{-# INLINE boundsForExactConversion #-}

minBoundAsInteger :: Bits i => i -> Maybe Integer
minBoundAsInteger dummyI = if isSigned dummyI then
                             case bitSizeMaybe dummyI of
                               Just bits -> Just (- bit (bits-1))
                               Nothing   -> Nothing
                           else
                             Just 0
{-# INLINE [1] minBoundAsInteger #-}
{-# RULES
"minBoundAsInteger/Int" minBoundAsInteger = (\_ -> Just (toInteger (minBound :: Int))) :: Int -> Maybe Integer
"minBoundAsInteger/Int8" minBoundAsInteger = (\_ -> Just (toInteger (minBound :: Int8))) :: Int8 -> Maybe Integer
"minBoundAsInteger/Int16" minBoundAsInteger = (\_ -> Just (toInteger (minBound :: Int16))) :: Int16 -> Maybe Integer
"minBoundAsInteger/Int32" minBoundAsInteger = (\_ -> Just (toInteger (minBound :: Int32))) :: Int32 -> Maybe Integer
"minBoundAsInteger/Int64" minBoundAsInteger = (\_ -> Just (toInteger (minBound :: Int64))) :: Int64 -> Maybe Integer
"minBoundAsInteger/Word" minBoundAsInteger = (\_ -> Just 0) :: Word -> Maybe Integer
"minBoundAsInteger/Word8" minBoundAsInteger = (\_ -> Just 0) :: Word8 -> Maybe Integer
"minBoundAsInteger/Word16" minBoundAsInteger = (\_ -> Just 0) :: Word16 -> Maybe Integer
"minBoundAsInteger/Word32" minBoundAsInteger = (\_ -> Just 0) :: Word32 -> Maybe Integer
"minBoundAsInteger/Word64" minBoundAsInteger = (\_ -> Just 0) :: Word64 -> Maybe Integer
  #-}

maxBoundAsInteger :: Bits i => i -> Maybe Integer
maxBoundAsInteger dummyI = case bitSizeMaybe dummyI of
                             Just bits | isSigned dummyI -> Just (bit (bits-1) - 1)
                                       | otherwise -> Just (bit bits - 1)
                             Nothing -> Nothing
{-# INLINE [1] maxBoundAsInteger #-}
{-# RULES
"maxBoundAsInteger/Int" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Int))) :: Int -> Maybe Integer
"maxBoundAsInteger/Int8" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Int8))) :: Int8 -> Maybe Integer
"maxBoundAsInteger/Int16" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Int16))) :: Int16 -> Maybe Integer
"maxBoundAsInteger/Int32" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Int32))) :: Int32 -> Maybe Integer
"maxBoundAsInteger/Int64" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Int64))) :: Int64 -> Maybe Integer
"maxBoundAsInteger/Word" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Word))) :: Word -> Maybe Integer
"maxBoundAsInteger/Word8" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Word8))) :: Word8 -> Maybe Integer
"maxBoundAsInteger/Word16" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Word16))) :: Word16 -> Maybe Integer
"maxBoundAsInteger/Word32" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Word32))) :: Word32 -> Maybe Integer
"maxBoundAsInteger/Word64" maxBoundAsInteger = (\_ -> Just (toInteger (maxBound :: Word64))) :: Word64 -> Maybe Integer
  #-}

-- Avoid cross-module specialization issue with manual worker/wrapper transformation
positiveWordToBinaryFloatR :: (RealFloat a, RoundingStrategy f) => Bool -> Word -> f a
positiveWordToBinaryFloatR neg (W# n#) = positiveWordToBinaryFloatR# neg n#
{-# INLINE positiveWordToBinaryFloatR #-}

positiveWordToBinaryFloatR# :: (RealFloat a, RoundingStrategy f) => Bool -> Word# -> f a
positiveWordToBinaryFloatR# !neg n# = result
  where
    n = W# n#
    result = let k = wordLog2' n -- floor (log2 n)
                 -- 2^k <= n < 2^(k+1) <= 2^(finiteBitSize n)
                 -- k <= finiteBitSize n - 1
             in if k < fDigits then
                  exact $ fromIntegral n
                else
                  -- expMax <= k implies expMax <= finiteBitSize n - 1
                  if expMax <= finiteBitSize n - 1 && k >= expMax then
                    -- overflow
                    let inf = 1 / 0
                    in inexact GT neg 1 maxFinite inf
                  else
                    -- k >= fDigits
                    let e = k - fDigits + 1 -- 1 <= e <= finiteBitSize n - fDigits
                        q = n `unsafeShiftR` e -- q <= n / 2^e = 2^(log2 n - (floor (log2 n) - fDigits + 1)) < 2^fDigits
                        r = n .&. (bit e - 1)
                        -- (q, r) = n `quotRem` (base^e)
                        -- base^(fDigits - 1) <= q < base^fDigits, 0 <= r < base^(k-fDigits+1)
                        towardzero_or_exact = fromIntegral (q `unsafeShiftL` e)
                        -- Although (q `unsafeShiftL` e) fits in Word, ((q + 1) `unsafeShiftL` e) may overflow.
                        -- fDigits + e = k + 1 <= WORD_SIZE_IN_BITS
                        -- Equality holds when wordLog2' n == WORD_SIZE_IN_BITS - 1, i.e. 2^(WORD_SIZE_IN_BITS - 1) <= n.
                        -- In particular,
                        -- * When q + 1 < 2^fDigits, (q + 1) * 2^e < 2^(fDigits + e) = 2^(k + 1) <= 2^WORD_SIZE_IN_BITS, so (q + 1) * 2^e does not overflow.
                        -- * When k + 1 < WORD_SIZE_IN_BITS, (q + 1) * 2^e <= 2^(fDigits + e) = 2^(k+1) < 2^WORD_SIZE_IN_BITS, so (q + 1) * 2^e does not overflow.
                        -- * q + 1 <= 2^fDigits and k + 1 <= WORD_SIZE_IN_BITS always hold.
                        -- * Therefore, ((q + 1) `unsafeShiftL` e) overflows only if q + 1 == 2^fDigits && k + 1 == WORD_SIZE_IN_BITS
                        awayfromzero = if q + 1 == bit fDigits && k == finiteBitSize n - 1 then
                                         -- (q + 1) `shiftL` e = 2^(fDigits + e) = 2^(k+1) = 2^(finiteBitSize n)
                                         encodeFloat 1 (finiteBitSize n)
                                       else
                                         fromIntegral ((q + 1) `unsafeShiftL` e)
                        parity = fromIntegral q :: Int
                    in doRound
                         (r == 0) -- exactness
                         (compare r (bit (e - 1)))
                         neg
                         parity
                         towardzero_or_exact
                         awayfromzero

    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (_expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double
{-# INLINABLE [0] positiveWordToBinaryFloatR# #-}
{-# SPECIALIZE
  positiveWordToBinaryFloatR# :: RoundingStrategy f => Bool -> Word# -> f Float
                               , RoundingStrategy f => Bool -> Word# -> f Double
                               , RealFloat a => Bool -> Word# -> RoundTiesToEven a
                               , RealFloat a => Bool -> Word# -> RoundTiesToAway a
                               , RealFloat a => Bool -> Word# -> RoundTowardPositive a
                               , RealFloat a => Bool -> Word# -> RoundTowardZero a
                               , RealFloat a => Bool -> Word# -> Product RoundTowardNegative RoundTowardPositive a
                               , Bool -> Word# -> RoundTiesToEven Float
                               , Bool -> Word# -> RoundTiesToAway Float
                               , Bool -> Word# -> RoundTowardPositive Float
                               , Bool -> Word# -> RoundTowardZero Float
                               , Bool -> Word# -> RoundTiesToEven Double
                               , Bool -> Word# -> RoundTiesToAway Double
                               , Bool -> Word# -> RoundTowardPositive Double
                               , Bool -> Word# -> RoundTowardZero Double
                               , Bool -> Word# -> Product RoundTowardNegative RoundTowardPositive Float
                               , Bool -> Word# -> Product RoundTowardNegative RoundTowardPositive Double
  #-}
{-# RULES
"positiveWordToBinaryFloatR#/RoundTowardNegative"
  positiveWordToBinaryFloatR# = \neg x -> RoundTowardNegative (roundTowardPositive (positiveWordToBinaryFloatR# (not neg) x))
  #-}

-- n > 0
fromPositiveIntegerR :: (RealFloat a, RoundingStrategy f) => Bool -> Integer -> f a
fromPositiveIntegerR !neg !n = assert (n > 0) result
  where
    result = let k = if base == 2 then
                       integerLog2' n
                     else
                       integerLogBase' base n -- floor (logBase base n)
                 -- base^k <= n < base^(k+1)
             in if k < fDigits then
                  exact $ fromInteger n
                else
                  if k >= expMax then
                    -- overflow
                    let inf = 1 / 0
                    in inexact GT neg 1 maxFinite inf
                  else
                    -- k >= fDigits
                    let e = k - fDigits + 1
                        -- k >= e (assuming fDigits >= 1)
                        -- Therefore, base^e <= n
                        (q, r) = quotRemByExpt n base e -- n `quotRem` (base^e)
                        -- base^(fDigits - 1) <= q < base^fDigits, 0 <= r < base^(k-fDigits+1)
                        towardzero_or_exact = encodeFloat q e
                        awayfromzero = encodeFloat (q + 1) e
                        parity = fromInteger q :: Int
                    in doRound
                         (isDivisibleByExpt n base e r) -- exactness (r == 0)
                         (compareWithExpt base n r (e - 1))
                         -- (compare r (expt base (e - 1)))
                         neg
                         parity
                         towardzero_or_exact
                         awayfromzero

    !base = floatRadix (undefined `asProxyTypeOf` result) -- 2 or 10
    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (_expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double
{-# INLINABLE [0] fromPositiveIntegerR #-}
{-# SPECIALIZE
  fromPositiveIntegerR :: RealFloat a => Bool -> Integer -> RoundTiesToEven a
                        , RealFloat a => Bool -> Integer -> RoundTiesToAway a
                        , RealFloat a => Bool -> Integer -> RoundTowardPositive a
                        , RealFloat a => Bool -> Integer -> RoundTowardZero a
                        , RealFloat a => Bool -> Integer -> Product RoundTowardNegative RoundTowardPositive a
                        , RoundingStrategy f => Bool -> Integer -> f Double
                        , RoundingStrategy f => Bool -> Integer -> f Float
                        , Bool -> Integer -> RoundTiesToEven Double
                        , Bool -> Integer -> RoundTiesToAway Double
                        , Bool -> Integer -> RoundTowardPositive Double
                        , Bool -> Integer -> RoundTowardZero Double
                        , Bool -> Integer -> RoundTiesToEven Float
                        , Bool -> Integer -> RoundTiesToAway Float
                        , Bool -> Integer -> RoundTowardPositive Float
                        , Bool -> Integer -> RoundTowardZero Float
                        , Bool -> Integer -> Product RoundTowardNegative RoundTowardPositive Double
                        , Bool -> Integer -> Product RoundTowardNegative RoundTowardPositive Float
  #-}
{-# RULES
"fromPositiveIntegerR/RoundTowardNegative"
  fromPositiveIntegerR = \neg x -> RoundTowardNegative (roundTowardPositive (fromPositiveIntegerR (not neg) x))
  #-}
