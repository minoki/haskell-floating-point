{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module Numeric.Floating.IEEE.Internal.Rounding where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Functor.Product
import           Data.Int
import           Data.Proxy
import           Data.Ratio
import           Data.Word
import           GHC.Exts
import           GHC.Float (expt)
import           Math.NumberTheory.Logarithms (integerLog2', integerLogBase',
                                               wordLog2')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base
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
    Numeric.Floating.IEEE.Internal.IntegerInternals.roundingMode n e
compareWithExpt base n r e = assert (r == n `rem` expt base (e+1)) $ compare r (expt base e)
{-# INLINE compareWithExpt #-}

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
                               Nothing -> Nothing
                           else
                             Just 0
{-# INLINE [1] minBoundAsInteger #-}
{-# RULES
"minBoundAsInteger/Int" minBoundAsInteger = (\_ -> Just (fromIntegral (minBound :: Int))) :: Int -> Maybe Integer
"minBoundAsInteger/Int8" minBoundAsInteger = (\_ -> Just (fromIntegral (minBound :: Int8))) :: Int8 -> Maybe Integer
"minBoundAsInteger/Int16" minBoundAsInteger = (\_ -> Just (fromIntegral (minBound :: Int16))) :: Int16 -> Maybe Integer
"minBoundAsInteger/Int32" minBoundAsInteger = (\_ -> Just (fromIntegral (minBound :: Int32))) :: Int32 -> Maybe Integer
"minBoundAsInteger/Int64" minBoundAsInteger = (\_ -> Just (fromIntegral (minBound :: Int64))) :: Int64 -> Maybe Integer
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
"maxBoundAsInteger/Int" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Int))) :: Int -> Maybe Integer
"maxBoundAsInteger/Int8" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Int8))) :: Int8 -> Maybe Integer
"maxBoundAsInteger/Int16" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Int16))) :: Int16 -> Maybe Integer
"maxBoundAsInteger/Int32" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Int32))) :: Int32 -> Maybe Integer
"maxBoundAsInteger/Int64" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Int64))) :: Int64 -> Maybe Integer
"maxBoundAsInteger/Word" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Word))) :: Word -> Maybe Integer
"maxBoundAsInteger/Word8" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Word8))) :: Word8 -> Maybe Integer
"maxBoundAsInteger/Word16" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Word16))) :: Word16 -> Maybe Integer
"maxBoundAsInteger/Word32" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Word32))) :: Word32 -> Maybe Integer
"maxBoundAsInteger/Word64" maxBoundAsInteger = (\_ -> Just (fromIntegral (maxBound :: Word64))) :: Word64 -> Maybe Integer
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

fromRationalR :: (RealFloat a, RoundingStrategy f) => Rational -> f a
fromRationalR x = fromRatioR (numerator x) (denominator x)
{-# INLINE fromRationalR #-}

fromRatioR :: (RealFloat a, RoundingStrategy f)
           => Integer -- ^ numerator
           -> Integer -- ^ denominator
           -> f a
fromRatioR 0 !_ = exact 0
fromRatioR n 0 | n > 0 = exact (1 / 0) -- positive infinity
               | otherwise = exact (- 1 / 0) -- negative infinity
fromRatioR n d | d < 0 = error "fromRatio: negative denominator"
               | n < 0 = negate <$> fromPositiveRatioR True (- n) d
               | otherwise = fromPositiveRatioR False n d
{-# INLINE fromRatioR #-}

fromPositiveRatioR :: (RealFloat a, RoundingStrategy f)
                   => Bool -- ^ True if the result will be negated
                   -> Integer -- ^ numerator (> 0)
                   -> Integer -- ^ denominator (> 0)
                   -> f a
fromPositiveRatioR !neg !n !d = assert (n > 0 && d > 0) result
  where
    result = let e0 :: Int
                 e0 = if base == 2 then
                        integerLog2' n - integerLog2' d - fDigits
                      else
                        integerLogBase' base n - integerLogBase' base d - fDigits
                 q0, r0, d0 :: Integer
                 (!d0, (!q0, !r0)) =
                   if e0 >= 0 then
                     -- n = q0 * (d * base^e0) + r0, 0 <= r0 < d * base^e0
                     let d_ = multiplyByExpt d base e0
                     in (d_, n `quotRem` d_)
                   else
                     -- n * base^(-e0) = q0 * d + r0, 0 <= r0 < d
                     (d, (multiplyByExpt n base (-e0)) `quotRem` d)
                 -- Invariant: n / d * base^^(-e0) = q0 + r0 / d0
                 !_ = assert (n % d * fromInteger base^^(-e0) == fromInteger q0 + r0 % d0) ()
                 !_ = assert (base^(fDigits-1) <= q0 && q0 < base^(fDigits+1)) ()

                 q, r, d' :: Integer
                 e :: Int
                 (!q, !r, !d', !e) =
                   if q0 < expt base fDigits then
                     -- base^(fDigits-1) <= q0 < base^fDigits
                     (q0, r0, d0, e0)
                   else
                     -- base^fDigits <= q0 < base^(fDigits+1)
                     let (q', r') = q0 `quotRem` base
                     in (q', r' * d0 + r0, base * d0, e0 + 1)
                 -- Invariant: n / d * 2^^(-e) = q + r / d', base^(fDigits-1) <= q < base^fDigits, 0 <= r < d'
                 !_ = assert (n % d * fromInteger base^^(-e) == fromInteger q + r % d') ()
                 -- base^(e+fDigits-1) <= q * base^^e <= n/d < (q+1) * base^^e <= base^(e+fDigits)
                 -- In particular, base^(fDigits-1) <= q < base^fDigits
             in if expMin <= e + fDigits && e + fDigits <= expMax then
                  -- normal: base^^(expMin-1) <= n/d < base^expMax
                  let towardzero_or_exact = encodeFloat q e
                      awayfromzero = encodeFloat (q + 1) e -- may be infinity
                      parity = fromInteger q :: Int
                  in doRound
                       (r == 0)
                       (compare (base * r) d')
                       neg
                       parity
                       towardzero_or_exact
                       awayfromzero
                else
                  if expMax < e + fDigits then
                    -- overflow
                    let inf = 1 / 0
                    in inexact GT neg 1 maxFinite inf
                  else
                    -- subnormal: 0 < n/d < base^^(expMin-1)
                    -- e + fDigits < expMin
                    let (q', r') = quotRemByExpt q base (expMin - fDigits - e)
                        !_ = assert (q == q' * base^(expMin-fDigits-e) + r' && 0 <= r' && r' < base^(expMin-fDigits-e)) ()
                        -- q = q' * base^(expMin-fDigits-e) + r', 0 <= r' < base^(expMin-fDigits-e)
                        -- n / d * base^^(-e) = q' * base^(expMin-fDigits-e) + r' + r / d'
                        -- n / d = q' * base^^(expMin - fDigits) + (r' + r / d') * base^^e
                        !_ = assert (n % d == fromInteger q' * fromInteger base^^(expMin - fDigits) + (fromInteger r' + r % d') * fromInteger base^^e) ()
                        -- rounding direction: (r' + r / d') * base^^e vs. base^^(expMin-fDigits-1)
                        towardzero = encodeFloat q' (expMin - fDigits)
                        awayfromzero = encodeFloat (q' + 1) (expMin - fDigits)
                        parity = fromInteger q' :: Int
                    in doRound
                         (r == 0 && r' == 0)
                         (compareWithExpt base q r' (expMin - fDigits - e - 1) <> if r == 0 then EQ else GT)
                         -- (compare r' (expt base (expMin - fDigits - e - 1)) <> if r == 0 then EQ else GT)
                         neg
                         parity
                         towardzero
                         awayfromzero

    !base = floatRadix (undefined `asProxyTypeOf` result)
    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double
{-# INLINABLE [0] fromPositiveRatioR #-}
{-# SPECIALIZE
  fromPositiveRatioR :: RealFloat a => Bool -> Integer -> Integer -> RoundTiesToEven a
                      , RealFloat a => Bool -> Integer -> Integer -> RoundTiesToAway a
                      , RealFloat a => Bool -> Integer -> Integer -> RoundTowardPositive a
                      , RealFloat a => Bool -> Integer -> Integer -> RoundTowardZero a
                      , RealFloat a => Bool -> Integer -> Integer -> Product RoundTowardNegative RoundTowardPositive a
                      , RoundingStrategy f => Bool -> Integer -> Integer -> f Double
                      , RoundingStrategy f => Bool -> Integer -> Integer -> f Float
                      , Bool -> Integer -> Integer -> RoundTiesToEven Double
                      , Bool -> Integer -> Integer -> RoundTiesToAway Double
                      , Bool -> Integer -> Integer -> RoundTowardPositive Double
                      , Bool -> Integer -> Integer -> RoundTowardZero Double
                      , Bool -> Integer -> Integer -> RoundTiesToEven Float
                      , Bool -> Integer -> Integer -> RoundTiesToAway Float
                      , Bool -> Integer -> Integer -> RoundTowardPositive Float
                      , Bool -> Integer -> Integer -> RoundTowardZero Float
                      , Bool -> Integer -> Integer -> Product RoundTowardNegative RoundTowardPositive Double
                      , Bool -> Integer -> Integer -> Product RoundTowardNegative RoundTowardPositive Float
  #-}
{-# RULES
"fromPositiveRatioR/RoundTowardNegative"
  fromPositiveRatioR = \neg x y -> RoundTowardNegative (roundTowardPositive (fromPositiveRatioR (not neg) x y))
  #-}

encodeFloatR :: (RealFloat a, RoundingStrategy f) => Integer -> Int -> f a
encodeFloatR 0 !_ = exact 0
encodeFloatR m n | m < 0 = negate <$> encodePositiveFloatR True (- m) n
                 | otherwise = encodePositiveFloatR False m n
{-# INLINE encodeFloatR #-}

-- Avoid cross-module specialization issue with manual worker/wrapper transformation
encodePositiveFloatR :: (RealFloat a, RoundingStrategy f) => Bool -> Integer -> Int -> f a
encodePositiveFloatR neg m (I# n#) = encodePositiveFloatR# neg m n#
{-# INLINE encodePositiveFloatR #-}

encodePositiveFloatR# :: (RealFloat a, RoundingStrategy f) => Bool -> Integer -> Int# -> f a
encodePositiveFloatR# !neg !m n# = assert (m > 0) result
  where
    n = I# n#
    result = let k = if base == 2 then
                       integerLog2' m
                     else
                       integerLogBase' base m
                 -- base^k <= m < base^(k+1)
                 -- base^^(k+n) <= m * base^^n < base^^(k+n+1)
             in if expMin <= k + n + 1 && k + n + 1 <= expMax then
                  -- normal
                  -- base^(fDigits-1) <= m / base^(k-fDigits+1) < base^fDigits
                  if k < fDigits then
                    -- m < base^(k+1) <= base^fDigits
                    exact $ encodeFloat m n
                  else
                    -- k >= fDigits
                    let (q,r) = quotRemByExpt m base (k - fDigits + 1)
                        -- m = q * base^^(k-fDigits+1) + r
                        -- base^(fDigits-1) <= q = m `quot` (base^^(k-fDigits+1)) < base^fDigits
                        -- m * base^^n = q * base^^(n+k-fDigits+1) + r * base^^n
                        towardzero_or_exact = encodeFloat q (n + k - fDigits + 1)
                        awayfromzero = encodeFloat (q + 1) (n + k - fDigits + 1)
                        parity = fromInteger q :: Int
                    in doRound
                         (isDivisibleByExpt m base (k - fDigits + 1) r) -- exactness (r == 0)
                         (compareWithExpt base m r (k - fDigits))
                         -- (compare r (expt base (k - fDigits)))
                         neg
                         parity
                         towardzero_or_exact
                         awayfromzero
                else
                  if expMax <= k + n then
                    -- overflow
                    let inf = 1 / 0
                    in inexact GT neg 1 maxFinite inf
                  else -- k + n + 1 < expMin
                    -- subnormal
                    if expMin - fDigits <= n then
                      -- k <= expMin - n <= fDigits
                      exact $ encodeFloat m n
                    else -- n < expMin - fDigits
                      -- k <= expMin - n, fDigits < expMin - n
                      let (q,r) = quotRemByExpt m base (expMin - fDigits - n)
                          -- m = q * base^(expMin-fDigits-n) + r
                          -- q <= m * base^^(n-expMin+fDigits) < q+1
                          -- q * base^^(expMin-fDigits) <= m * base^^n < (q+1) * base^^(expMin-fDigits)
                          !_ = assert (toRational q * toRational base^^(expMin-fDigits) <= toRational m * toRational base^^n) ()
                          !_ = assert (toRational m * toRational base^^n < toRational (q+1) * toRational base^^(expMin-fDigits)) ()
                          towardzero_or_exact = encodeFloat q (expMin - fDigits)
                          awayfromzero = encodeFloat (q + 1) (expMin - fDigits)
                          parity = fromInteger q :: Int
                      in doRound
                           (isDivisibleByExpt m base (expMin - fDigits - n) r) -- exactness (r == 0)
                           (compareWithExpt base m r (expMin - fDigits - n - 1))
                           -- (compare r (expt base (expMin - fDigits - n - 1)))
                           neg
                           parity
                           towardzero_or_exact
                           awayfromzero

    !base = floatRadix (undefined `asProxyTypeOf` result)
    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double
{-# INLINABLE [0] encodePositiveFloatR# #-}
{-# SPECIALIZE
  encodePositiveFloatR# :: RealFloat a => Bool -> Integer -> Int# -> RoundTiesToEven a
                         , RealFloat a => Bool -> Integer -> Int# -> RoundTiesToAway a
                         , RealFloat a => Bool -> Integer -> Int# -> RoundTowardPositive a
                         , RealFloat a => Bool -> Integer -> Int# -> RoundTowardZero a
                         , RealFloat a => Bool -> Integer -> Int# -> Product RoundTowardNegative RoundTowardPositive a
                         , RoundingStrategy f => Bool -> Integer -> Int# -> f Double
                         , RoundingStrategy f => Bool -> Integer -> Int# -> f Float
                         , Bool -> Integer -> Int# -> RoundTiesToEven Double
                         , Bool -> Integer -> Int# -> RoundTiesToAway Double
                         , Bool -> Integer -> Int# -> RoundTowardPositive Double
                         , Bool -> Integer -> Int# -> RoundTowardZero Double
                         , Bool -> Integer -> Int# -> RoundTiesToEven Float
                         , Bool -> Integer -> Int# -> RoundTiesToAway Float
                         , Bool -> Integer -> Int# -> RoundTowardPositive Float
                         , Bool -> Integer -> Int# -> RoundTowardZero Float
                         , Bool -> Integer -> Int# -> Product RoundTowardNegative RoundTowardPositive Double
                         , Bool -> Integer -> Int# -> Product RoundTowardNegative RoundTowardPositive Float
  #-}
{-# RULES
"encodePositiveFloatR#/RoundTowardNegative"
  encodePositiveFloatR# = \neg x y -> RoundTowardNegative (roundTowardPositive (encodePositiveFloatR# (not neg) x y))
  #-}
