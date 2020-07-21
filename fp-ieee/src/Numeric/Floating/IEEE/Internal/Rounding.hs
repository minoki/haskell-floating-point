{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Rounding where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Functor.Product
import           Data.Proxy (asProxyTypeOf)
import           Data.Ratio
import           GHC.Float (expt)
import           Math.NumberTheory.Logarithms (integerLog2', integerLogBase')
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Base

default ()

class Functor f => RoundingStrategy f where
  exact :: a -> f a
  inexactNotTie :: Bool -- ^ negative (True -> negative, False -> positive)
                -> Int -- ^ parity (even -> toward-zero is even, odd -> toward-zero is odd)
                -> a -- ^ nearest
                -> a -- ^ toward zero
                -> a -- ^ away from zero
                -> f a
  inexactTie :: Bool -- ^ negative (True -> negative, False -> positive)
             -> Int -- ^ parity (even -> toward-zero is even, odd -> toward-zero is odd)
             -> a -- ^ toward zero
             -> a -- ^ away from zero
             -> f a

newtype RoundTiesToEven a = RoundTiesToEven { roundTiesToEven :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundTiesToEven where
  exact = RoundTiesToEven
  inexactNotTie _neg _parity near _zero _away = RoundTiesToEven near
  inexactTie _neg parity zero away | even parity = RoundTiesToEven zero
                                   | otherwise = RoundTiesToEven away
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

newtype RoundTiesToAway a = RoundTiesToAway { roundTiesToAway :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundTiesToAway where
  exact = RoundTiesToAway
  inexactNotTie _neg _parity near _zero _away = RoundTiesToAway near
  inexactTie _neg _parity _zero away = RoundTiesToAway away
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

newtype RoundTowardPositive a = RoundTowardPositive { roundTowardPositive :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundTowardPositive where
  exact = RoundTowardPositive
  inexactNotTie neg _parity _near zero away | neg = RoundTowardPositive zero
                                            | otherwise = RoundTowardPositive away
  inexactTie neg _parity zero away | neg = RoundTowardPositive zero
                                   | otherwise = RoundTowardPositive away
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

newtype RoundTowardNegative a = RoundTowardNegative { roundTowardNegative :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundTowardNegative where
  exact = RoundTowardNegative
  inexactNotTie neg _parity _near zero away | neg = RoundTowardNegative away
                                            | otherwise = RoundTowardNegative zero
  inexactTie neg _parity zero away | neg = RoundTowardNegative away
                                   | otherwise = RoundTowardNegative zero
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

newtype RoundTowardZero a = RoundTowardZero { roundTowardZero :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundTowardZero where
  exact = RoundTowardZero
  inexactNotTie _neg _parity _near zero _away = RoundTowardZero zero
  inexactTie _neg _parity zero _away = RoundTowardZero zero
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

newtype RoundTiesTowardZero a = RoundTiesTowardZero { roundTiesTowardZero :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundTiesTowardZero where
  exact = RoundTiesTowardZero
  inexactNotTie _neg _parity near _zero _away = RoundTiesTowardZero near
  inexactTie _neg _parity zero _away = RoundTiesTowardZero zero

newtype RoundToOdd a = RoundToOdd { roundToOdd :: a }
  deriving (Eq,Show,Functor)

instance RoundingStrategy RoundToOdd where
  exact = RoundToOdd
  inexactNotTie _neg parity _near zero away | even parity = RoundToOdd away
                                            | otherwise = RoundToOdd zero
  inexactTie _neg parity zero away | even parity = RoundToOdd away
                                   | otherwise = RoundToOdd zero

newtype Exactness a = Exactness { isExact :: Bool }
  deriving (Eq,Show,Functor)

instance RoundingStrategy Exactness where
  exact _ = Exactness True
  inexactNotTie _neg _parity _near _zero _away = Exactness False
  inexactTie _neg _parity _zero _away = Exactness False
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

instance (RoundingStrategy f, RoundingStrategy g) => RoundingStrategy (Product f g) where
  exact x = Pair (exact x) (exact x)
  inexactNotTie neg parity near zero away = Pair (inexactNotTie neg parity near zero away) (inexactNotTie neg parity near zero away)
  inexactTie neg parity zero away = Pair (inexactTie neg parity zero away) (inexactTie neg parity zero away)
  {-# INLINE exact #-}
  {-# INLINE inexactNotTie #-}
  {-# INLINE inexactTie #-}

{-
fromIntegerR :: (RealFloat a, RoundingStrategy f) => Integer -> f a
fromRationalR :: (RealFloat a, RoundingStrategy f) => Rational -> f a
encodeFloatR :: (RealFloat a, RoundingStrategy f) => Integer -> Int -> f a
scaleFloatR :: (RealFloat a, RoundingStrategy f) => Int -> a -> f a
floatToInteger :: (RealFloat a, RoundingStrategy f) => a -> f Integer
floatToInteger' :: (RealFloat a, RoundingStrategy f) => a -> f a
-}

{-
from GHC.Float:
expt :: Integer -> Int -> Integer
expt base n = base ^ n
-}

quotRemByExpt :: Integer -> Integer -> Int -> (Integer, Integer)
quotRemByExpt x 2 n    = (x `shiftR` n, x .&. (bit n - 1))
quotRemByExpt x base n = x `quotRem` expt base n
{-# INLINE quotRemByExpt #-}

multiplyByExpt :: Integer -> Integer -> Int -> Integer
multiplyByExpt x 2 n    = x `shiftL` n
multiplyByExpt x base n = x * expt base n
{-# INLINE multiplyByExpt #-}

divideByExpt :: Integer -> Integer -> Int -> Integer
divideByExpt x 2 n    = x `shiftR` n
divideByExpt x base n = x `quot` expt base n
{-# INLINE divideByExpt #-}

fromIntegerR :: (RealFloat a, RoundingStrategy f) => Integer -> f a
fromIntegerR 0 = exact 0
fromIntegerR n | n < 0 = negate <$> fromPositiveIntegerR True (- n)
               | otherwise = fromPositiveIntegerR False n
{-# INLINE fromIntegerR #-}

-- |
-- IEEE 754 @convertFromInt@ operation, with each rounding attributes.
fromIntegerTiesToEven, fromIntegerTiesToAway, fromIntegerTowardPositive, fromIntegerTowardNegative, fromIntegerTowardZero :: RealFloat a => Integer -> a
fromIntegerTiesToEven = roundTiesToEven . fromIntegerR
fromIntegerTiesToAway = roundTiesToAway . fromIntegerR
fromIntegerTowardPositive = roundTowardPositive . fromIntegerR
fromIntegerTowardNegative = roundTowardNegative . fromIntegerR
fromIntegerTowardZero = roundTowardZero . fromIntegerR
{-# INLINE fromIntegerTiesToEven #-}
{-# INLINE fromIntegerTiesToAway #-}
{-# INLINE fromIntegerTowardPositive #-}
{-# INLINE fromIntegerTowardNegative #-}
{-# INLINE fromIntegerTowardZero #-}

matchOrdering :: Ordering -> a -> a -> a -> a
matchOrdering ordering x y z = case ordering of
                                 LT -> x
                                 EQ -> y
                                 GT -> z
{-# INLINE [0] matchOrdering #-}
{-# RULES
"matchOrdering" forall o x. matchOrdering o x x x = x
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
                    in inexactNotTie
                         neg
                         1 -- parity
                         inf -- near
                         maxFinite -- zero
                         inf -- away
                  else
                    let e = k - fDigits + 1
                        (q, r) = quotRemByExpt n base e -- n `quotRem` (base^e)
                        -- base^(fDigits - 1) <= q < base^fDigits, 0 <= r < base^(k-fDigits+1)
                    in if r == 0 then
                         exact $ encodeFloat q e
                       else
                         -- inexact
                         let down = encodeFloat q e
                             up = encodeFloat (q + 1) e
                             parity = fromInteger q :: Int
                         in matchOrdering (compare r (expt base (e - 1)))
                            -- LT ->
                            (inexactNotTie
                              neg
                              parity
                              down -- near
                              down -- zero
                              up -- away
                            )
                            -- EQ ->
                            (inexactTie
                              neg
                              parity
                              down -- zero
                              up -- away
                            )
                            -- GT ->
                            (inexactNotTie
                              neg
                              parity
                              up -- near
                              down -- zero
                              up -- away
                            )

    !base = floatRadix (undefined `asProxyTypeOf` result) -- 2 or 10
    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (_expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double
{-# SPECIALIZE [0] fromPositiveIntegerR
                     :: RealFloat a => Bool -> Integer -> RoundTiesToEven a
                      , RealFloat a => Bool -> Integer -> RoundTiesToAway a
                      , RealFloat a => Bool -> Integer -> RoundTowardPositive a
                      , RealFloat a => Bool -> Integer -> RoundTowardNegative a
                      , RealFloat a => Bool -> Integer -> RoundTowardZero a
                      , RoundingStrategy f => Bool -> Integer -> f Double
                      , RoundingStrategy f => Bool -> Integer -> f Float
  #-}
{-# SPECIALIZE fromPositiveIntegerR
                 :: Bool -> Integer -> RoundTiesToEven Double
                  , Bool -> Integer -> RoundTiesToAway Double
                  , Bool -> Integer -> RoundTowardPositive Double
                  , Bool -> Integer -> RoundTowardNegative Double
                  , Bool -> Integer -> RoundTowardZero Double
                  , Bool -> Integer -> RoundTiesToEven Float
                  , Bool -> Integer -> RoundTiesToAway Float
                  , Bool -> Integer -> RoundTowardPositive Float
                  , Bool -> Integer -> RoundTowardNegative Float
                  , Bool -> Integer -> RoundTowardZero Float
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

fromPositiveRatioR :: (RealFloat a, RoundingStrategy f)
                   => Bool -- ^ True if the result will be negated
                   -> Integer -- ^ numerator
                   -> Integer -- ^ denominator
                   -> f a
fromPositiveRatioR !neg !n !d = assert (n > 0 && d > 0) result
  where
    result = let e0 :: Int
                 e0 = integerLogBase' base n - integerLogBase' base d - fDigits
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
             in if expMin <= e + fDigits && e + fDigits <= expMax then
                  -- normal: base^^(expMin-1) <= n/d < base^expMax
                  if r == 0 then
                    exact $ encodeFloat q e
                  else
                    -- inexact
                    let down = encodeFloat q e
                        up = encodeFloat (q + 1) e -- may be infinity
                        parity = fromInteger q :: Int
                    in case compare (base * r) d' of
                         LT -> inexactNotTie
                                 neg
                                 parity
                                 down -- near
                                 down -- zero
                                 up -- away
                         EQ -> inexactTie
                                 neg
                                 parity
                                 down -- zero
                                 up -- away
                         GT -> inexactNotTie
                                 neg
                                 parity
                                 up -- near
                                 down -- zero
                                 up -- away
                else
                  if expMax < e + fDigits then
                    -- overflow
                    let inf = 1 / 0
                    in inexactNotTie
                         neg
                         1 -- parity
                         inf -- near
                         maxFinite -- zero
                         inf -- away
                  else
                    -- subnormal: 0 < n/d < base^^(expMin-1)
                    -- e + fDigits < expMin
                    let (!q', !r') = quotRemByExpt q base (expMin - fDigits - e)
                        -- q = q' * base^(expMin-fDigits-e) + r', 0 <= r' < base^(expMin-fDigits-e)
                        -- n / d * base^^(-e) = q' * base^(expMin-fDigits-e) + r' + r / d'
                        -- n / d = q' * base^^(expMin - fDigits) + (r' + r / d') * base^^e
                    in if r == 0 && r' == 0 then
                         exact $ encodeFloat q' (expMin - fDigits)
                       else
                         -- (r' + r / d') * base^^e vs. base^^(expMin-fDigits-1)
                         let down = encodeFloat q' (expMin - fDigits)
                             up = encodeFloat (q' + 1) (expMin - fDigits)
                             parity = fromInteger q' :: Int
                         in case compare r' (expt base (expMin - fDigits - e - 1)) of
                              LT -> inexactNotTie
                                      neg
                                      parity
                                      down -- near
                                      down -- zero
                                      up -- away
                              EQ -> if r == 0 then
                                      inexactTie
                                        neg
                                        parity
                                        down -- zero
                                        up -- away
                                    else
                                      inexactNotTie
                                        neg
                                        parity
                                        up -- near
                                        down -- zero
                                        up -- away
                              GT -> inexactNotTie
                                      neg
                                      parity
                                      up -- near
                                      down -- zero
                                      up -- away

    !base = floatRadix (undefined `asProxyTypeOf` result)
    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double

encodeFloatR :: (RealFloat a, RoundingStrategy f) => Integer -> Int -> f a
encodeFloatR 0 !_ = exact 0
encodeFloatR m n | m < 0 = negate <$> encodePositiveFloatR True (- m) n
                 | otherwise = encodePositiveFloatR False m n

encodePositiveFloatR :: (RealFloat a, RoundingStrategy f) => Bool -> Integer -> Int -> f a
encodePositiveFloatR !neg !m !n = assert (m > 0) result
  where
    result = let k = integerLogBase' base m
                 -- base^k <= m < base^(k+1)
                 -- base^^(k+n) <= m * base^^n < base^^(k+n+1)
             in if expMin < k + n && k + n < expMax then
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
                    in if r == 0 then
                         exact $ encodeFloat q (n + k - fDigits + 1)
                       else
                         -- inexact
                         let down = encodeFloat q (n + k - fDigits + 1)
                             up = encodeFloat (q + 1) (n + k - fDigits + 1)
                             parity = fromInteger q :: Int
                         in case compare r (expt base (k - fDigits)) of
                              LT -> inexactNotTie
                                      neg
                                      parity
                                      down -- near
                                      down -- zero
                                      up -- away
                              EQ -> inexactTie
                                      neg
                                      parity
                                      down -- zero
                                      up -- away
                              GT -> inexactNotTie
                                      neg
                                      parity
                                      up -- near
                                      down -- zero
                                      up -- away
                else
                  if expMax <= k + n then
                    -- overflow
                    let inf = 1 / 0
                    in inexactNotTie
                         neg
                         1 -- parity
                         inf -- near
                         maxFinite -- zero
                         inf -- away
                  else -- k + n <= expMin
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
                      in if r == 0 then
                           exact $ encodeFloat q (expMin - fDigits)
                         else
                           -- inexact
                           let down = encodeFloat q (expMin - fDigits)
                               up = encodeFloat (q + 1) (expMin - fDigits)
                               parity = fromInteger q :: Int
                           in case compare r (expt base (expMin - fDigits - n - 1)) of
                                LT -> inexactNotTie
                                        neg
                                        parity
                                        down -- near
                                        down -- zero
                                        up -- away
                                EQ -> inexactTie
                                        neg
                                        parity
                                        down -- zero
                                        up -- away
                                GT -> inexactNotTie
                                        neg
                                        parity
                                        up -- near
                                        down -- zero
                                        up -- away

    !base = floatRadix (undefined `asProxyTypeOf` result)
    !fDigits = floatDigits (undefined `asProxyTypeOf` result) -- 53 for Double
    (!expMin, !expMax) = floatRange (undefined `asProxyTypeOf` result) -- (-1021, 1024) for Double
