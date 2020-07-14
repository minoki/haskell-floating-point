{-|
Module      : Numeric.Floating.Extra.IEEE
Description : IEEE 754-compliant operations for floating-point numbers

This module provides IEEE 754-compliant operations for floating-point numbers.

The functions in this module assume that the given floating-point type conform to IEEE 754 format.

Since 'RealFloat' constraint is insufficient to query properties of a NaN, the functions here assumes all NaN as positive, quiet.
If you want better treatment for NaNs, use the module "Numeric.Floating.Extra.IEEE.NaN".

Since floating-point exceptions cannot be accessed from Haskell in normal way, the operations provided by this module ignore exceptional behavior.
Don't let fp exceptions trap.

On i386 target, you may need to set @-msse2@ option to get correct floating-point behavior.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Floating.Extra.IEEE
  (
  -- * 5.3 Homogeneous general-computational operations
  --
  -- ** 5.3.1 General operations
    roundToIntegralTiesToEven
  , roundToIntegralTiesToAway
  , roundToIntegralTowardZero
  , roundToIntegralTowardPositive
  , roundToIntegralTowardNegative
  -- | @roundToIntegralExact{...}@: not implemented yet
  , nextUp
  , nextDown
  -- | 'nextTowardZero' is not in IEEE, but may be useful to some.
  , nextTowardZero -- not in IEEE
  , remainder
  -- , c_remainderFloat
  -- , c_remainderDouble

  -- ** 5.3.2 Decimal operations (not supported)
  --
  -- | Not supported.

  -- ** 5.3.3 logBFormat operations
  --
  -- | Not supported.
  , scaleFloat -- scaleB
  -- logB x = exponent x - 1

  -- * 5.4 formatOf general-computational operations
  --
  -- ** 5.4.1 Arithmetic operations
  --
  , (+) -- addition
  , (-) -- subtraction
  , (*) -- multiplication
  , (/) -- division
  , sqrt -- squareRoot
  , fusedMultiplyAdd
  , fusedMultiplyAdd_viaRational
  , fusedMultiplyAdd_viaInteger
  , fusedMultiplyAdd_twoProduct
  , fusedMultiplyAddFloat_viaDouble
  -- , c_fusedMultiplyAddFloat
  -- , c_fusedMultiplyAddDouble
  -- |
  -- @convertFromInt@: not implemented yet
  , round    -- convertToIntegerTiesToEven: round
  , roundTiesToAway
  , truncate -- convertToIntegerTowardZero: truncate
  , ceiling  -- convertToIntegerTowardPositive: ceiling
  , floor    -- convertToIntegerTowardNegative: floor

  -- ** 5.4.2 Conversion operations for floating-point formats and decimal character sequences
  --
  , realFloatToFrac -- convertFormat
  -- |
  -- @convertFromDecimalCharacter@: not implemented. readSigned readFloat?
  --
  -- convertToDecimalCharacter: not implemented. show(E|F|G)Float?

  -- * 5.4.3 Conversion operations for binary formats
  --
  -- |
  -- convertFromHexCharacter: not implemented. readHexFloat?
  --
  -- convertToHexCharacter: not implemented. show(E|F|G)Float?

  -- * 5.5 Quiet-computational operations
  --
  -- ** 5.5.1 Sign bit operations
  -- copy: id
  , negate
  , abs
  -- | copySign: not implemented

  -- ** 5.5.2 Decimal re-encoding operations (not supported)
  --
  -- |
  -- Not supported.

  -- * 5.6 Signaling-computational operations
  --
  -- ** 5.6.1 Comparisons (not supported)
  --
  -- |
  -- This library does not support floating-point exceptions.

  -- * 5.7 Non-computational operations
  --
  -- ** 5.7.1 Conformance predicates (not supported)
  --
  -- |
  -- Not supported.

  -- ** 5.7.2 General operations
  , Class(..)
  , classify -- class
  , isSignMinus
  , isNormal
  , isFinite
  , isZero
  , isDenormalized -- isSubnormal
  , isInfinite -- re-export
  , isNaN -- re-export
  -- |
  -- isSignaling: not supported here.
  -- isCanonical: not supported.
  , floatRadix -- radix
  , compareByTotalOrder -- totalOrder
  , compareByTotalOrderMag -- totalOrderMag

  -- ** 5.7.3 Decimal operation (not supported)
  --
  -- |
  -- Not supported.

  -- ** 5.7.4 Operations on subsets of flags (not supported)
  --
  -- |
  -- Not supported.

  -- * 9. Recommended operations

  -- * 9.5 Augmented arithmetic operations
  , augmentedAddition
  , augmentedSubtraction
  , augmentedMultiplication
  , augmentedAddition_viaRational
  , augmentedMultiplication_viaRational

  -- * 9.6 Minimum and maximum operations
  , minimum'
  , minimumNumber
  , maximum'
  , maximumNumber
  , minimumMagnitude
  , minimumMagnitudeNumber
  , maximumMagnitude
  , maximumMagnitudeNumber

  -- * Uncategorized
  , minPositive
  , maxFinite
  , distanceUlp
  , twoSum
  , twoProduct
  , twoProduct_nonscaling
  , split
  , isMantissaEven
  , roundTiesTowardZero
  ) where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Ratio
import           GHC.Float.Compat (castDoubleToWord64, castFloatToWord32,
                            castWord32ToFloat, castWord64ToDouble,
                            double2Float, float2Double, isDoubleFinite, isFloatFinite)
import           Math.NumberTheory.Logarithms (integerLog2')
import           MyPrelude
import           Numeric.Floating.Extra.Conversion

default ()

-- $setup
-- >>> :set -XHexFloatLiterals -XNumericUnderscores

isFloatBinary32 :: Bool
isFloatBinary32 = isIEEE x
                  && floatRadix x == 2
                  && floatDigits x == 24
                  && floatRange x == (-125, 128)
  where x :: Float
        x = undefined

isDoubleBinary64 :: Bool
isDoubleBinary64 = isIEEE x
                   && floatRadix x == 2
                   && floatDigits x == 53
                   && floatRange x == (-1021, 1024)
  where x :: Double
        x = undefined

-- |
-- prop> (minPositive :: Float) == 0x1p-149
-- prop> (minPositive :: Double) == 0x1p-1074
-- prop> nextDown (minPositive :: Float) == 0
-- prop> nextDown (minPositive :: Double) == 0
minPositive :: RealFloat a => a
minPositive = let d = floatDigits x
                  (expMin,_expMax) = floatRange x
                  x = encodeFloat 1 (expMin - d)
              in x
{-# INLINABLE minPositive #-}
{-# SPECIALIZE minPositive :: Float, Double #-}

-- TODO: minPositiveNormal?

-- |
-- prop> (maxFinite :: Float) == 0x1.fffffep+127
-- prop> (maxFinite :: Double) == 0x1.ffff_ffff_ffff_fp+1023
maxFinite :: RealFloat a => a
maxFinite = let d = floatDigits x
                (_expMin,expMax) = floatRange x
                r = floatRadix x
                x = encodeFloat (r ^! d - 1) (expMax - d)
            in x
{-# INLINABLE maxFinite #-}
{-# SPECIALIZE maxFinite :: Float, Double #-}

-- A variant of (^) allowing constant folding for base = 2
infixr 8 ^!
(^!) :: Integer -> Int -> Integer
(^!) = (^)
{-# INLINE [2] (^!) #-}
{-# RULES
"2^!" forall y. 2 ^! y = staticIf (y >= 0) (1 `shiftL` y) (2 ^ y)
  #-}

staticIf :: Bool -> a -> a -> a
staticIf _ _ x = x
{-# INLINE [0] staticIf #-}
{-# RULES
"staticIf/True" forall x y. staticIf True x y = x
"staticIf/False" forall x y. staticIf False x y = y
  #-}

-- |
-- IEEE 754 @nextUp@ operation.
--
-- prop> nextUp 1 == (0x1.000002p0 :: Float)
-- prop> nextUp 1 == (0x1.0000_0000_0000_1p0 :: Double)
-- prop> nextUp (1/0) == (1/0 :: Double)
-- prop> nextUp (-1/0) == (- maxFinite :: Double)
-- prop> nextUp 0 == (0x1p-1074 :: Double)
-- prop> nextUp (-0) == (0x1p-1074 :: Double)
-- prop> nextUp (-0x1p-1074) == (-0 :: Double)
-- prop> isNegativeZero (nextUp (-0x1p-1074) :: Double)
nextUp :: RealFloat a => a -> a
nextUp x | not (isIEEE x) = error "non-IEEE numbers are not supported"
         | floatRadix x /= 2 = error "non-binary types are not supported" -- TODO
         | isNaN x || (isInfinite x && x > 0) = x -- NaN or positive infinity
         | x >= 0 = nextUp_positive x
         | otherwise = - nextDown_positive (- x)
{-# INLINE [1] nextUp #-}

-- |
-- IEEE 754 @nextDown@ operation.
--
-- prop> nextDown 1 == (0x1.ffff_ffff_ffff_fp-1 :: Double)
-- prop> nextDown 1 == (0x1.fffffep-1 :: Float)
-- prop> nextDown (1/0) == (maxFinite :: Double)
-- prop> nextDown (-1/0) == (-1/0 :: Double)
-- prop> nextDown 0 == (-0x1p-1074 :: Double)
-- prop> nextDown (-0) == (-0x1p-1074 :: Double)
-- prop> nextDown 0x1p-1074 == (0 :: Double)
-- prop> nextDown 0x1p-1022 == (0x0.ffff_ffff_ffff_fp-1022 ::Double)
nextDown :: RealFloat a => a -> a
nextDown x | not (isIEEE x) = error "non-IEEE numbers are not supported"
           | floatRadix x /= 2 = error "non-binary types are not supported" -- TODO
           | isNaN x || (isInfinite x && x < 0) = x -- NaN or negative infinity
           | x >= 0 = nextDown_positive x
           | otherwise = - nextUp_positive (- x)
{-# INLINE [1] nextDown #-}

-- |
-- prop> nextTowardZero 1 == (0x1.ffff_ffff_ffff_fp-1 :: Double)
-- prop> nextTowardZero 1 == (0x1.fffffep-1 :: Float)
-- prop> nextTowardZero (1/0) == (maxFinite :: Double)
-- prop> nextTowardZero (-1/0) == (-maxFinite :: Double)
-- prop> nextTowardZero 0 == (0 :: Double)
-- prop> isNegativeZero (nextTowardZero (-0 :: Double))
-- prop> nextTowardZero 0x1p-1074 == (0 :: Double)
nextTowardZero :: RealFloat a => a -> a
nextTowardZero x | not (isIEEE x) = error "non-IEEE numbers are not supported"
                 | floatRadix x /= 2 = error "non-binary types are not supported" -- TODO
                 | isNaN x || x == 0 = x -- NaN or zero
                 | x >= 0 = nextDown_positive x
                 | otherwise = - nextDown_positive (- x)
{-# INLINE [1] nextTowardZero #-}

nextUp_positive :: RealFloat a => a -> a
nextUp_positive x
  | isNaN x || x < 0 = error "nextUp_positive"
  | isInfinite x = x
  | x == 0 = encodeFloat 1 (expMin - d) -- min positive
  | otherwise = let m :: Integer
                    e :: Int
                    (m,e) = decodeFloat x
                    -- x = m * 2^e, 2^(d-1) <= m < 2^d
                    -- 2^expMin < x < 2^expMax
                    -- 2^(expMin-d): min positive
                    -- 2^(expMin - 1): min normal 0x1p-1022
                    -- expMin - d <= e <= expMax - d (-1074 .. 971)
                in if expMin - d <= e
                   then encodeFloat (m + 1) e -- normal
                   else let m' = m `shiftR` (expMin - d - e)
                        in encodeFloat (m' + 1) (expMin - d) -- subnormal
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,_expMax) = floatRange x -- (-1021,1024) for Double
{-# INLINE nextUp_positive #-}

nextDown_positive :: RealFloat a => a -> a
nextDown_positive x
  | isNaN x || x < 0 = error "nextDown_positive"
  | isInfinite x = encodeFloat ((1 `unsafeShiftL` d) - 1) (expMax - d) -- max finite
  | x == 0 = encodeFloat (-1) (expMin - d) -- max negative
  | otherwise = let m :: Integer
                    e :: Int
                    (m,e) = decodeFloat x
                    -- x = m * 2^e, 2^(d-1) <= m < 2^d
                    -- 2^expMin < x < 2^expMax
                    -- 2^(expMin-d): min positive
                    -- 2^(expMin - 1): min normal 0x1p-1022
                    -- expMin - d <= e <= expMax - d (-1074 .. 971)
                in if expMin - d <= e
                   then -- normal
                     let m1 = m - 1
                     in if m .&. m1 == 0 && expMin - d /= e
                        then encodeFloat (2 * m - 1) (e - 1)
                        else encodeFloat m1 e
                   else -- subnormal
                     let m' = m `shiftR` (expMin - d - e)
                     in encodeFloat (m' - 1) (expMin - d)
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
{-# INLINE nextDown_positive #-}

{-# RULES
"nextUp/Float" nextUp = nextUpFloat
"nextUp/Double" nextUp = nextUpDouble
"nextDown/Float" nextDown = nextDownFloat
"nextDown/Double" nextDown = nextDownDouble
"nextTowardZero/Float" nextTowardZero = nextTowardZeroFloat
"nextTowardZero/Double" nextTowardZero = nextTowardZeroDouble
  #-}

-- |
-- prop> nextUpFloat 1 == 0x1.000002p0
-- prop> nextUpFloat (1/0) == 1/0
-- prop> nextUpFloat (-1/0) == - maxFinite
-- prop> nextUpFloat 0 == 0x1p-149
-- prop> nextUpFloat (-0) == 0x1p-149
-- prop> isNegativeZero (nextUpFloat (-0x1p-149))
nextUpFloat :: Float -> Float
nextUpFloat x =
  case castFloatToWord32 x of
    w | w .&. 0x7f80_0000 == 0x7f80_0000
      , w /= 0xff80_0000 -> x -- NaN or positive infinity -> itself
    0x8000_0000 -> minPositive -- -0 -> min positive
    w | testBit w 31 -> castWord32ToFloat (w - 1) -- negative
      | otherwise -> castWord32ToFloat (w + 1) -- positive
  where
    !True = isFloatBinary32 || error "Numeric.Floating.Extra assumes Float is IEEE binary32"

-- |
-- prop> nextUpDouble 1 == 0x1.0000_0000_0000_1p0
-- prop> nextUpDouble (1/0) == 1/0
-- prop> nextUpDouble (-1/0) == - maxFinite
-- prop> nextUpDouble 0 == 0x1p-1074
-- prop> nextUpDouble (-0) == 0x1p-1074
-- prop> isNegativeZero (nextUpDouble (-0x1p-1074))
nextUpDouble :: Double -> Double
nextUpDouble x =
  case castDoubleToWord64 x of
    w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
      , w /= 0xfff0_0000_0000_0000 -> x -- NaN or positive infinity -> itself
    0x8000_0000_0000_0000 -> minPositive -- -0 -> min positive
    w | testBit w 63 -> castWord64ToDouble (w - 1) -- negative
      | otherwise -> castWord64ToDouble (w + 1) -- positive
  where
     !True = isDoubleBinary64 || error "Numeric.Floating.Extra assumes Double is IEEE binary64"

-- |
-- prop> nextDownFloat 1 == 0x1.fffffep-1
-- prop> nextDownFloat (1/0) == maxFinite
-- prop> nextDownFloat (-1/0) == -1/0
-- prop> nextDownFloat 0 == -0x1p-149
-- prop> nextDownFloat (-0) == -0x1p-149
-- prop> nextDownFloat 0x1p-149 == 0
nextDownFloat :: Float -> Float
nextDownFloat x =
  case castFloatToWord32 x of
    w | w .&. 0x7f80_0000 == 0x7f80_0000
      , w /= 0x7f80_0000 -> x -- NaN or negative infinity -> itself
    0x0000_0000 -> - minPositive -- +0 -> max negative
    w | testBit w 31 -> castWord32ToFloat (w + 1) -- negative
      | otherwise -> castWord32ToFloat (w - 1) -- positive
  where
    !True = isFloatBinary32 || error "Numeric.Floating.Extra assumes Float is IEEE binary32"

-- |
-- prop> nextDownDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextDownDouble (1/0) == maxFinite
-- prop> nextDownDouble (-1/0) == -1/0
-- prop> nextDownDouble 0 == -0x1p-1074
-- prop> nextDownDouble (-0) == -0x1p-1074
-- prop> nextDownDouble 0x1p-1074 == 0
nextDownDouble :: Double -> Double
nextDownDouble x =
  case castDoubleToWord64 x of
    w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
      , w /= 0x7ff0_0000_0000_0000 -> x -- NaN or negative infinity -> itself
    0x0000_0000_0000_0000 -> - minPositive -- +0 -> max negative
    w | testBit w 63 -> castWord64ToDouble (w + 1) -- negative
      | otherwise -> castWord64ToDouble (w - 1) -- positive
  where
     !True = isDoubleBinary64 || error "Numeric.Floating.Extra assumes Double is IEEE binary64"

-- |
-- prop> nextTowardZeroFloat 1 == 0x1.fffffep-1
-- prop> nextTowardZeroFloat (-1) == -0x1.fffffep-1
-- prop> nextTowardZeroFloat (1/0) == maxFinite
-- prop> nextTowardZeroFloat (-1/0) == -maxFinite
-- prop> nextTowardZeroFloat 0 == 0
-- prop> isNegativeZero (nextTowardZeroFloat (-0))
-- prop> nextTowardZeroFloat 0x1p-149 == 0
nextTowardZeroFloat :: Float -> Float
nextTowardZeroFloat x =
  case castFloatToWord32 x of
    w | w .&. 0x7f80_0000 == 0x7f80_0000
      , w .&. 0x007f_ffff /= 0 -> x -- NaN -> itself
    0x8000_0000 -> x -- -0 -> itself
    0x0000_0000 -> x -- +0 -> itself
    w -> castWord32ToFloat (w - 1) -- positive / negative
  where
    !True = isFloatBinary32 || error "Numeric.Floating.Extra assumes Float is IEEE binary32"

-- |
-- prop> nextTowardZeroDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (-1) == -0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (1/0) == maxFinite
-- prop> nextTowardZeroDouble (-1/0) == -maxFinite
-- prop> nextTowardZeroDouble 0 == 0
-- prop> isNegativeZero (nextTowardZeroDouble (-0))
-- prop> nextTowardZeroDouble 0x1p-1074 == 0
nextTowardZeroDouble :: Double -> Double
nextTowardZeroDouble x =
  case castDoubleToWord64 x of
    w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
      , w .&. 0x000f_ffff_ffff_ffff /= 0 -> x -- NaN -> itself
    0x8000_0000_0000_0000 -> x -- -0 -> itself
    0x0000_0000_0000_0000 -> x -- +0 -> itself
    w -> castWord64ToDouble (w - 1) -- positive / negative
  where
    !True = isDoubleBinary64 || error "Numeric.Floating.Extra assumes Double is IEEE binary64"

-- Assumption: input is finite
isMantissaEven :: RealFloat a => a -> Bool
isMantissaEven 0 = True
isMantissaEven x = let !_ = assert (isFinite x) ()
                       (m,n) = decodeFloat x
                       d = floatDigits x
                       !_ = assert (floatRadix x ^ (d - 1) <= abs m && abs m < floatRadix x ^ d) ()
                       (expMin, _expMax) = floatRange x
                       s = expMin - (n + d)
                       !_ = assert (isDenormalized x == (s > 0)) ()
                   in if s > 0 then
                        even (m `shiftR` s)
                      else
                        even m
{-# NOINLINE [1] isMantissaEven #-}
{-# RULES
"isMantissaEven/Double" forall (x :: Double).
  isMantissaEven x = even (castDoubleToWord64 x)
"isMantissaEven/Float" forall (x :: Float).
  isMantissaEven x = even (castFloatToWord32 x)
  #-}

-- |
-- prop> \a b -> case twoSum a b of (x, y) -> a + b == x && toRational a + toRational b == toRational x + toRational y
twoSum :: Num a => a -> a -> (a, a)
twoSum a b =
  let x = a + b
      t = x - a
      y = (a - (x - t)) + (b - t)
  in (x, y)
{-# SPECIALIZE twoSum :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

-- Addition, with round to nearest odd floating-point number
add_roundToOdd :: RealFloat a => a -> a -> a
add_roundToOdd x y = let (u, v) = twoSum x y
                         result | v < 0 && isMantissaEven u = nextDown u
                                | v > 0 && isMantissaEven u = nextUp u
                                | otherwise = u
                         !_ = assert (toRational u == toRational x + toRational y || not (isMantissaEven result)) ()
                     in result
{-# SPECIALIZE add_roundToOdd :: Float -> Float -> Float, Double -> Double -> Double #-}

-- |
-- prop> \a b -> case twoProduct a b of (x, y) -> a * b == x && fromRational (toRational a * toRational b - toRational x) == y
twoProduct :: RealFloat a => a -> a -> (a, a)
twoProduct a b =
  let eab = exponent a + exponent b
      a' = significand a
      b' = significand b
      (ah, al) = split a'
      (bh, bl) = split b'
      x = a * b -- Since 'significand' doesn't honor the sign of zero, we can't use @a' * b'@
      y' = al * bl - (scaleFloat (-eab) x - ah * bh - al * bh - ah * bl)
  in (x, scaleFloat eab y')
{-# SPECIALIZE twoProduct :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

{-
twoProduct_Float :: Float -> Float -> (Float, Float)
twoProduct_Float a b =
  let x, y :: Float
      a', b', x' :: Double
      a' = float2Double a
      b' = float2Double b
      x' = a' * b'
      x = double2Float x'
      y = double2Float (x' - float2Double x)
  in (x, y)
-}

twoProduct_nonscaling :: RealFloat a => a -> a -> (a, a)
twoProduct_nonscaling a b =
  let (ah, al) = split a
      (bh, bl) = split b
      x = a * b
      y = al * bl - (x - ah * bh - al * bh - ah * bl)
  in (x, y)
{-# SPECIALIZE twoProduct_nonscaling :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

-- This function doesn't handle overflow or underflow
split :: RealFloat a => a -> (a, a)
split a =
  let c = factor * a
      x = c - (c - a)
      y = a - x
  in (x, y)
  where factor = fromInteger $ 1 + floatRadix a ^! ((floatDigits a + 1) `quot` 2)
  -- factor == 134217729 for Double, 4097 for Float
{-# SPECIALIZE split :: Float -> (Float, Float), Double -> (Double, Double) #-}

-- |
-- prop> \a b c -> toRational (fma_twoProd a b c) == toRational a * toRational b + toRational c
fusedMultiplyAdd_twoProduct :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_twoProduct a b c
  | isFinite a && isFinite b && isFinite c =
    let eab | a == 0 || b == 0 = fst (floatRange a) - floatDigits a -- reasonably small
            | otherwise = exponent a + exponent b
        ec | c == 0 = fst (floatRange c) - floatDigits c
           | otherwise = exponent c

        -- Avoid overflow in twoProduct
        a' = significand a
        b' = significand b
        (x', y') = twoProduct_nonscaling a' b'
        !_ = assert (toRational a' * toRational b' == toRational x' + toRational y') ()

        -- Avoid overflow in twoSum
        e = max eab ec
        x = scaleFloat (eab - e) x'
        y = scaleFloat (eab - e) y'
        c'' = scaleFloat (max (fst (floatRange c) - floatDigits c + 1) (ec - e) - ec) c -- may be inexact

        (u1,u2) = twoSum y c''
        (v1,v2) = twoSum u1 x
        w = add_roundToOdd u2 v2
        result0 = v1 + w
        !_ = assert (result0 == fromRational (toRational x + toRational y + toRational c'')) ()
        result = scaleFloat e result0
        !_ = assert (result == fromRational (toRational a * toRational b + toRational c) || isDenormalized result) ()
    in if result0 == 0 then
         -- We need to handle the sign of zero
         if c == 0 && a /= 0 && b /= 0 then
           a * b -- let a * b underflow
         else
           a * b + c -- -0 if both a * b and c are -0
       else
         if isDenormalized result then
           -- The rounding in 'scaleFloat e result0' may yield an incorrect result.
           -- Take the slow path.
           case toRational a * toRational b + toRational c of
             0 -> a * b + c -- This should be exact
             r -> fromRational r
         else
           result
  | isFinite a && isFinite b = c -- c is +-Infinity or NaN
  | otherwise = a * b + c -- Infinity or NaN
{-# SPECIALIZE fusedMultiplyAdd_twoProduct :: Float -> Float -> Float -> Float, Double -> Double -> Double -> Double #-}

-- TODO: Check if this works correctly
fusedMultiplyAddFloat_viaDouble :: Float -> Float -> Float -> Float
fusedMultiplyAddFloat_viaDouble a b c
  | isFinite a && isFinite b && isFinite c =
    let a', b', c' :: Double
        a' = float2Double a
        b' = float2Double b
        c' = float2Double c
        ab = a' * b' -- exact
        !_ = assert (toRational ab == toRational a' * toRational b') ()
        result = double2Float (add_roundToOdd ab c')
        !_ = assert (result == fromRational (toRational a * toRational b + toRational c)) ()
    in result
  | isFinite a && isFinite b = c -- a * b is finite, but c is Infinity or NaN
  | otherwise = a * b + c
  where
    !True = isFloatBinary32 || error "fusedMultiplyAdd/Float: Float must be IEEE binary32"
    !True = isDoubleBinary64 || error "fusedMultiplyAdd/Float: Double must be IEEE binary64"

fusedMultiplyAdd_viaInteger :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_viaInteger x y z
  | isFinite x && isFinite y && isFinite z =
      let (mx,ex) = decodeFloat x -- x == mx * b^ex, mx==0 || b^(d-1) <= abs mx < b^d
          (my,ey) = decodeFloat y -- y == my * b^ey, my==0 || b^(d-1) <= abs my < b^d
          (mz,ez) = decodeFloat z -- z == mz * b^ez, mz==0 || b^(d-1) <= abs mz < b^d
          exy = ex + ey
          ee = min ez exy
          !2 = floatRadix x
      in case mx * my `shiftL` (exy - ee) + mz `shiftL` (ez - ee) of
           0 -> x * y + z
           m -> encodeFloat m ee -- TODO: correct rounding
  | isFinite x && isFinite y = z -- x * y is finite, but z is Infinity or NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN
{-# NOINLINE [1] fusedMultiplyAdd_viaInteger #-}

fusedMultiplyAdd_viaRational :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd_viaRational x y z
  | isFinite x && isFinite y && isFinite z =
      case toRational x * toRational y + toRational z of
        0 -> x * y + z
        r -> fromRational r
  | isFinite x && isFinite y = z -- x * is finite, but z is Infinity or NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN

-- |
-- IEEE 754 @fusedMultiplyAdd@ operation.
fusedMultiplyAdd :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd = fusedMultiplyAdd_twoProduct
{-# NOINLINE [1] fusedMultiplyAdd #-}

#ifdef USE_FFI

-- libm's fma might be implemented with hardware
foreign import ccall unsafe "fmaf"
  c_fusedMultiplyAddFloat :: Float -> Float -> Float -> Float
foreign import ccall unsafe "fma"
  c_fusedMultiplyAddDouble :: Double -> Double -> Double -> Double

{-# RULES
"fusedMultiplyAdd/Float" fusedMultiplyAdd = c_fusedMultiplyAddFloat
"fusedMultiplyAdd/Double" fusedMultiplyAdd = c_fusedMultiplyAddDouble
  #-}

#else

{-# RULES
"fusedMultiplyAdd/Float" fusedMultiplyAdd = fusedMultiplyAddFloat_viaDouble
"fusedMultiplyAdd/Double" fusedMultiplyAdd = fusedMultiplyAdd_twoProduct :: Double -> Double -> Double -> Double
  #-}

#endif

-- generic, heterogeneous?
-- to narrower type
-- narrowingAdd
-- narrowingSub
-- narrowingMul
-- narrowingDiv
-- narrowingSqrt
-- narrowingFMA

-- TODO: rounding version, exception-aware version
-- TODO: check floatRadix are equal?
-- TODO: Move to it's own module?
infixl 6 `addition`, `subtraction`
infixl 7 `multiplication`, `division`
-- type family FloatRadix a :: Nat
-- addition :: (RealFloat a, RealFloat b, FloatRadix a ~ FloatRadix b) => a -> a -> b
addition :: (RealFloat a, RealFloat b) => a -> a -> b
addition x y = fromRational (toRational x + toRational y) -- TODO: NaN, inf, signed zero
{-# NOINLINE [1] addition #-}
{-# RULES
"addition/a->a" addition = (+)
  #-}
{-
-- to Half:
{-# SPECIALIZE addition :: Float -> Float -> Half #-} -- USE_HALF
{-# SPECIALIZE addition :: Double -> Double -> Half #-} -- USE_HALF
{-# SPECIALIZE addition :: LongDouble -> LongDouble -> Half #-} -- USE_HALF && USE_LONGDOUBLE
{-# SPECIALIZE addition :: Float128 -> Float128 -> Half #-} -- USE_HALF && USE_FLOAT128
-- to Float
{-# SPECIALIZE addition :: Double -> Double -> Float #-}
{-# SPECIALIZE addition :: LongDouble -> LongDouble -> Float #-} -- USE_LONGDOUBLE
{-# SPECIALIZE addition :: Float128 -> Float128 -> Float #-} -- USE_FLOAT128
-- to Double:
{-# SPECIALIZE addition :: LongDouble -> LongDouble -> Double #-} -- USE_LONGDOUBLE
{-# SPECIALIZE addition :: Float128 -> Float128 -> Double #-} -- USE_FLOAT128
-- to LongDouble:
{-# SPECIALIZE addition :: Float128 -> Float128 -> LongDouble #-} -- USE_LONGDOUBLE && USE_FLOAT128
-}
subtraction :: (RealFloat a, RealFloat b) => a -> a -> b
subtraction x y = fromRational (toRational x - toRational y) -- TODO: NaN, inf, signed zero
multiplication :: (RealFloat a, RealFloat b) => a -> a -> b
multiplication x y = fromRational (toRational x * toRational y) -- TODO: NaN, inf, signed zero
division :: (RealFloat a, RealFloat b) => a -> a -> b
division x y = fromRational (toRational x / toRational y) -- TODO: NaN, inf, signed zero
squareRoot :: (RealFloat a, RealFloat b) => a -> b
squareRoot x | x == 0 = realFloatToFrac x
squareRoot x = error "not implemented yet"
genericFusedMultiplyAdd :: (RealFloat a, RealFloat b) => a -> a -> a -> b
genericFusedMultiplyAdd x y z
  | isNaN x || isNaN y || isNaN z || isInfinite x || isInfinite y || isInfinite z = realFloatToFrac (x * y + z)
  | otherwise = case toRational x * toRational y + toRational z of
                  0 | isNegativeZero (x * y + z) -> -0
                  r -> fromRational r

distanceUlp :: RealFloat a => a -> a -> Maybe Integer
distanceUlp x y
  | isInfinite x || isInfinite y || isNaN x || isNaN y = Nothing
  | otherwise = let m = min (abs x) (abs y)
                    m' = nextUp m
                    v = (toRational y - toRational x) / toRational (m' - m)
                in if denominator v == 1
                   then Just (abs (numerator v))
                   else error "distanceUlp"

-- |
-- IEEE 754 @remainder@ operation.
remainder :: RealFloat a => a -> a -> a
remainder x y | isFinite x && isInfinite y = x
              | y == 0 || isInfinite y || isNaN y || not (isFinite x) = (x - x) / y * y -- return a NaN
              | otherwise = let n = round (toRational x / toRational y) -- TODO: Is round (x / y) okay?
                                r = x - y * fromInteger n
                            in r -- if r == 0, the sign of r is the same as x
{-# NOINLINE [1] remainder #-}

#ifdef USE_FFI
foreign import ccall unsafe "remainderf"
  c_remainderFloat :: Float -> Float -> Float
foreign import ccall unsafe "remainder"
  c_remainderDouble :: Double -> Double -> Double

{-# RULES
"remainder/Float" remainder = c_remainderFloat
"remainder/Double" remainder = c_remainderDouble
  #-}
#endif

-- |
-- IEEE 754 @roundToIntegralTiesToEven@ operation.
--
-- prop> \x -> roundToIntegralTiesToEven x == fromInteger (round x)
-- prop> isNegativeZero (roundToIntegralTiesToEven (-0.5))
roundToIntegralTiesToEven :: RealFloat a => a -> a
roundToIntegralTiesToEven x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTiesToEven x = case round x of
                                0 | x < 0 -> -0
                                  | otherwise -> 0
                                n -> fromInteger n
{-# NOINLINE [1] roundToIntegralTiesToEven #-}

-- |
-- IEEE 754 @roundToIntegralTiesToAway@ operation.
--
-- prop> \x -> roundToIntegralTiesToAway x == fromInteger (roundTiesToAway x)
-- prop> isNegativeZero (roundToIntegralTiesToAway (-0.4))
roundToIntegralTiesToAway :: RealFloat a => a -> a
roundToIntegralTiesToAway x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTiesToAway x = case properFraction x of
                                -- x == n + f, signum x == signum f, 0 <= abs f < 1
                                (n,r) -> if abs r < 0.5 then
                                           -- round toward zero
                                           if n == 0 then
                                             0.0 * r -- signed zero
                                           else
                                             fromInteger n
                                         else
                                           -- round away from zero
                                           if r < 0 then
                                             fromInteger (n - 1)
                                           else
                                             fromInteger (n + 1)
{-# NOINLINE [1] roundToIntegralTiesToAway #-}

-- |
-- IEEE 754 @roundToIntegralTowardZero@ operation.
--
-- prop> \x -> roundToIntegralTowardZero x == fromInteger (truncate x)
-- prop> isNegativeZero (roundToIntegralTowardZero (-0.5))
roundToIntegralTowardZero :: RealFloat a => a -> a
roundToIntegralTowardZero x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTowardZero x = case truncate x of
                                0 | x < 0 -> -0
                                  | otherwise -> 0
                                n -> fromInteger n
{-# NOINLINE [1] roundToIntegralTowardZero #-}

-- |
-- IEEE 754 @roundToIntegralTowardPositive@ operation.
--
-- prop> \x -> roundToIntegralTowardPositive x == fromInteger (ceiling x)
-- prop> isNegativeZero (roundToIntegralTowardPositive (-0.5))
roundToIntegralTowardPositive :: RealFloat a => a -> a
roundToIntegralTowardPositive x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTowardPositive x = case ceiling x of
                                    0 | x < 0 -> -0
                                      | otherwise -> 0
                                    n -> fromInteger n
{-# NOINLINE [1] roundToIntegralTowardPositive #-}

-- |
-- IEEE 754 @roundToIntegralTowardNegative@ operation.
--
-- prop> \x -> roundToIntegralTowardNegative x == fromInteger (floor x)
-- prop> isNegativeZero (roundToIntegralTowardNegative (-0))
roundToIntegralTowardNegative :: RealFloat a => a -> a
roundToIntegralTowardNegative x | isInfinite x || isNaN x || isNegativeZero x = x
                                | otherwise = fromInteger (floor x)
{-# NOINLINE [1] roundToIntegralTowardNegative #-}

-- |
-- IEEE 754 @convertToIntegerTiesToAway@ operation.
roundTiesToAway :: (RealFrac a, Integral b) => a -> b
roundTiesToAway x = case properFraction x of
                      -- x == n + f, signum x == signum f, 0 <= abs f < 1
                      (n,r) -> if abs r < 0.5 then
                                 n
                               else
                                 if r < 0 then
                                   n - 1
                                 else
                                   n + 1
{-# INLINE roundTiesToAway #-}

#ifdef USE_FFI

foreign import ccall unsafe "ceilf"
  c_ceilFloat :: Float -> Float
foreign import ccall unsafe "ceil"
  c_ceilDouble :: Double -> Double
foreign import ccall unsafe "floorf"
  c_floorFloat :: Float -> Float
foreign import ccall unsafe "floor"
  c_floorDouble :: Double -> Double
foreign import ccall unsafe "roundf"
  c_roundFloat :: Float -> Float -- ties to away
foreign import ccall unsafe "round"
  c_roundDouble :: Double -> Double -- ties to away
foreign import ccall unsafe "truncf"
  c_truncFloat :: Float -> Float
foreign import ccall unsafe "floor"
  c_truncDouble :: Double -> Double

-- TODO: Rules for roundToIntegralTiesToEven
-- nearbyint or roundeven (C2x)
{- from base
foreign import ccall unsafe "rintFloat"
  c_rintFloat :: Float -> Float
foreign import ccall unsafe "rintDouble"
  c_rintDouble :: Double -> Double
-}
#if defined(HAS_FAST_ROUNDEVEN)
foreign import ccall unsafe "hs_roundevenFloat"
  c_roundevenFloat :: Float -> Float
foreign import ccall unsafe "hs_roundevenDouble"
  c_roundevenDouble :: Double -> Double
#endif

{-# RULES
"roundToIntegralTiesToAway/Float"
  roundToIntegralTiesToAway = c_roundFloat
"roundToIntegralTiesToAway/Double"
  roundToIntegralTiesToAway = c_roundDouble
"roundToIntegralTowardZero/Float"
  roundToIntegralTowardZero = c_truncFloat
"roundToIntegralTowardZero/Double"
  roundToIntegralTowardZero = c_truncDouble
"roundToIntegralTowardPositive/Float"
  roundToIntegralTowardPositive = c_ceilFloat
"roundToIntegralTowardPositive/Double"
  roundToIntegralTowardPositive = c_ceilDouble
"roundToIntegralTowardNegative/Float"
  roundToIntegralTowardNegative = c_floorFloat
"roundToIntegralTowardNegative/Double"
  roundToIntegralTowardNegative = c_floorDouble
  #-}

#endif

-- roundToIntegral{TiesToEven,TiesToAway,TowardZero,TowardPositive,TowardNegative} -> round',_,truncate',ceiling',floor' :: a -> a
-- roundToIntegralExact -> not supported (maybe fp-exceptions)
-- Return @Maybe a@ or @(a, Bool)@?
-- nextUp, nextDown: done
-- remainder
-- Decimal operations:
--  quantize
--  quantum
-- logBFormat operations
--   scaleB
--   logB
-- Arithmetic
--   formatOf-{addition, subtraction, multiplication, division, squareRoot, fusedMultiplyAdd, convertFromInt}
--   intFormatOf-convertToInteger{TiesToEven,TowardZero,TowardPositive,TowardNegative,TiesToAway} -> round,_,truncate,ceiling,floor :: Integral b => a -> b
--   intFormatOf-convertToIntegerExact{TiesToEven,TowardZero,TowardPositive,TowardNegative,TiesToAway} -> not supported
-- fp-decimal
--   formatOf-convertFormat
--   formatOf-convertFromDecimalCharacter
--   convertToDecimalCharacter
-- binary
--   convertFromHexCharacter
--   convertToHexCharacter
-- sign bit
--   copy
--   negate
--   abs
--   copySign
-- Decimal re-encoding
--   encodeDecimal
--   decodeDecimal
--   encodeBinary
--   decodeBinary
-- comparison
--   compareQuiet{Equal,NotEqual,Greater,GreaterEqual,Less,LessEqual,Unordered,NotGreater,LessUnordered,NotLess,GreaterUnordered,Ordered}
--   compareSignaling{Equal,NotEqual,Greater,GreaterEqual,Less,LessEqual,NotGreater,LessUnordered,NotLess,GreaterUnordered}
-- is754version1985
-- is754version2008
-- is754version2019
-- General
--   class
--   isSignMinus: Sign of 0, sign of NaN
--   isNormal
--   isFinite
--   isZero --> (== 0)
--   isSubnormal -> isDenormalized
--   isInfinite -> isInfinite
--   isNaN -> isNaN
--   isSignaling
--   isCanonical
--   radix -> floatRadix
--   totalOrder *
--   totalOrderMag *
-- sameQuantum
-- lowerFlags, raiseFlags, testFlags, testSavedFlags, restoreFlags, saveAllFlags

-- |
-- IEEE 754 @isNormal@ operation.
isNormal :: RealFloat a => a -> Bool
isNormal x = x /= 0 && not (isNaN x) && not (isInfinite x) && not (isDenormalized x)
{-# NOINLINE [1] isNormal #-}
{-# RULES
"isNormal/Float" isNormal = isFloatNormal
"isNormal/Double" isNormal = isDoubleNormal
  #-}

isFloatNormal :: Float -> Bool
isFloatNormal x = let w = castFloatToWord32 x .&. 0x7f80_0000
                  in w /= 0 && w /= 0x7f80_0000

isDoubleNormal :: Double -> Bool
isDoubleNormal x = let w = castDoubleToWord64 x .&. 0x7ff0_0000_0000_0000
                   in w /= 0 && w /= 0x7ff0_0000_0000_0000

-- |
-- IEEE 754 @isFinite@ operation.
isFinite :: RealFloat a => a -> Bool
isFinite x = not (isNaN x) && not (isInfinite x)
{-# NOINLINE [1] isFinite #-}
{-# RULES
"isFinite/Float" forall (x :: Float).
  isFinite x = isFloatFinite x /= 0
"isFinite/Double" forall (x :: Double).
  isFinite x = isDoubleFinite x /= 0
  #-}

-- |
-- IEEE 754 @isZero@ operation.
isZero :: RealFloat a => a -> Bool
isZero x = x == 0

-- |
-- IEEE 754 @isSignMinus@ operation.
--
-- Since 'RealFloat' constraint is insufficient to query the sign of NaNs,
-- this function treats all NaNs as positive.
isSignMinus :: RealFloat a => a -> Bool
isSignMinus x = x < 0 || isNegativeZero x

-- |
-- Comparison with IEEE 754 @totalOrder@ predicate.
--
-- Since 'RealFloat' constraint is insufficient to query the sign and payload of NaNs,
-- this function treats all NaNs as positive.
--
-- Floating-point numbers are ordered as,
-- \(-\infty < \text{negative reals} < -0 < +0 < \text{positive reals} < +\infty < \mathrm{NaN}\).
compareByTotalOrder :: RealFloat a => a -> a -> Ordering
compareByTotalOrder x y
  | x < y = LT
  | y < x = GT
  | x == y = if x == 0 then
               compare (isNegativeZero y) (isNegativeZero x)
             else
               EQ
  | otherwise = compare (isNaN x) (isNaN y) -- The sign bit and payload of NaNs are ignored
-- TODO: Specialize for Float, Double

-- |
-- Comparison with IEEE 754 @totalOrderMag@ predicate.
compareByTotalOrderMag :: RealFloat a => a -> a -> Ordering
compareByTotalOrderMag x y = compareByTotalOrder (abs x) (abs y)

-- isCanonical :: a -> Bool

-- data PartialOrdering = LT | EQ | GT | UNORD

data Class = SignalingNaN
           | QuietNaN
           | NegativeInfinity
           | NegativeNormal
           | NegativeSubnormal
           | NegativeZero
           | PositiveZero
           | PositiveSubnormal
           | PositiveNormal
           | PositiveInfinity
           deriving (Eq, Ord, Show, Read, Enum)

-- | Treats NaNs as quiet
classify :: RealFloat a => a -> Class
classify x | isNaN x                 = QuietNaN
           | x < 0, isInfinite x     = NegativeInfinity
           | x < 0, isDenormalized x = NegativeSubnormal
           | x < 0                   = NegativeNormal
           | isNegativeZero x        = NegativeZero
           | x == 0                  = PositiveZero
           | isDenormalized x        = PositiveSubnormal
           | isInfinite x            = PositiveInfinity
           | otherwise               = PositiveNormal
{-# NOINLINE [1] classify #-}
{-# RULES
"classify/Double" classify = classifyDouble
  #-}

classifyDouble :: Double -> Class
classifyDouble x = let w = castDoubleToWord64 x
                       s = testBit w 63 -- sign
                       e = (w `unsafeShiftR` 52) .&. 0x7ff -- exponent
                       m = w .&. 0x000f_ffff_ffff_ffff -- mantissa
                   in case (s, e, m) of
                        (True,  0,     0) -> NegativeZero
                        (False, 0,     0) -> PositiveZero
                        (True,  0,     _) -> NegativeSubnormal
                        (False, 0,     _) -> PositiveSubnormal
                        (True,  0x7ff, 0) -> NegativeInfinity
                        (False, 0x7ff, 0) -> PositiveInfinity
                        (_,     0x7ff, _) -> QuietNaN -- treat all NaNs as quiet
                        (True,  _,     _) -> NegativeNormal
                        (False, _,     _) -> PositiveNormal

-- TODO (recommended):
-- exp
-- expm1
-- exp2
-- exp2m1
-- exp10
-- exp10m1
-- log
-- log2
-- log10
-- logp1
-- log2p1
-- log10p1
-- hypot
-- rSqrt
-- compound
-- rootn
-- pown
-- pow
-- powr
-- sin
-- cos
-- tan
-- sinPi
-- cosPi
-- tanPi
-- asin
-- acos
-- atan
-- atan2
-- asinPi
-- acosPi
-- atanPi
-- atan2Pi
-- sinh
-- cosh
-- tanh
-- asinh
-- acosh
-- atanh
-- getBinaryRoundingDirection -- not supported
-- setBinaryRoundingDirection -- not supported
-- getDecimalRoundingDirection -- not supported
-- setDecimalRoundingDirection -- not supported
-- saveModes
-- restoreModes
-- defaultModes
-- sum
-- dot
-- sumSquare
-- sumAbs
-- scaledProd
-- scaledProdSum
-- scaledProdDiff
-- augmentedAddition
-- augmentedSubtraction
-- augmentedMultiplication
-- minimum
-- minimumNumber
-- maximum
-- maximumNumber
-- minimumMagnitude
-- minimumMagnitudeNumber
-- maximumMagnitude
-- maximumMagnitudeNumber
-- getPayload
-- setPayload
-- setPayloadSignaling

-- |
-- IEEE 754 @augmentedAddition@ operation.
augmentedAddition :: RealFloat a => a -> a -> (a, a)
augmentedAddition !x !y
  | isNaN x || isInfinite x || isNaN y || isInfinite y = let !result = x + y in (result, result)
  | otherwise = let (u1, u2) = twoSum x y
                    ulpTowardZero = u1 - nextTowardZero u1
                in if isInfinite u1 then
                     -- Handle undue overflow: e.g. 0x1.ffff_ffff_ffff_f8p1023
                     handleUndueOverflow
                   else
                     if u2 == 0 then
                       (u1, 0 * u1) -- signed zero
                     else
                       if (-2) * u2 == ulpTowardZero then
                         (u1 - ulpTowardZero, ulpTowardZero + u2)
                       else
                         (u1, u2)
  where
    handleUndueOverflow =
      let e = max (exponent x) (exponent y)
          x' = scaleFloat (- e) x
          y' = scaleFloat (- e) y
          (u1, u2) = twoSum x' y'
          ulpTowardZero = u1 - nextTowardZero u1
          (v1, v2) | (-2) * u2 == ulpTowardZero = (u1 - ulpTowardZero, ulpTowardZero + u2)
                   | otherwise = (u1, u2)
          r1 = scaleFloat e v1
          r2 = scaleFloat e v2
      in if isInfinite r1 then
           (r1, r1) -- unavoidable overflow
         else
           assert (r2 /= 0) (r1, r2)

-- |
-- IEEE 754 @augmentedSubtraction@ operation.
augmentedSubtraction :: RealFloat a => a -> a -> (a, a)
augmentedSubtraction x y = augmentedAddition x (negate y)

-- |
-- IEEE 754 @augmentedMultiplication@ operation.
augmentedMultiplication :: RealFloat a => a -> a -> (a, a)
augmentedMultiplication !x !y
  | isNaN x || isInfinite x || isNaN y || isInfinite y || x * y == 0 = let !result = x * y in (result, result)
  | otherwise = let exy = exponent x + exponent y
                    x' = significand x
                    y' = significand y
                    (u1, u2) = twoProduct_nonscaling x' y'
                    !_ = assert (toRational x' * toRational y' == toRational u1 + toRational u2) ()
                    -- The product is subnormal <=> exy + exponent u1 < expMin
                    -- The product is inexact => exy + exponent u1 < expMin + d
                in if exy + exponent u1 >= expMin then
                     -- The result is exact
                     let ulpTowardZero = u1 - nextTowardZero u1
                         !_ = assert (2 * abs u2 <= abs ulpTowardZero) ()
                         (v1, v2) = if (-2) * u2 == ulpTowardZero then
                                      (u1 - ulpTowardZero, ulpTowardZero + u2)
                                    else
                                      (u1, u2)
                         !_ = assert (v1 + v2 == u1 + u2) ()
                         r1 = scaleFloat exy v1
                         !_ = assert (r1 == roundTiesTowardZero (toRational x * toRational y)) ()
                     in if isInfinite r1 then
                          (r1, r1)
                        else
                          if v2 == 0 then
                            (r1, 0 * r1) -- signed zero
                          else
                            if exy >= expMin + d then
                              -- The result is exact
                              let r2 = scaleFloat exy v2
                              in (r1, r2)
                            else
                              -- The upper part is normal, the lower is subnormal (and inexact)
                              -- Compute 'scaleFloat exy v2' with roundTiesTowardZero
                              let !r2 = scaleFloatIntoSubnormalTiesTowardZero exy v2
                                  !_ = assert (r2 == roundTiesTowardZero (toRational x * toRational y - toRational r1)) ()
                              in (r1, r2)
                   else
                     -- The upper part is subnormal (possibly inexact), and the lower is signed zero (possibly inexact)
                     if u2 == 0 then
                       -- u1 is exact
                       let !_ = assert (toRational x' * toRational y' == toRational u1) ()
                           r1 = scaleFloatIntoSubnormalTiesTowardZero exy u1
                           r1' = scaleFloat (-exy) r1
                       in if u1 == r1' then
                            (r1, 0 * r1)
                          else
                            (r1, 0 * (u1 - r1'))
                     else
                       let u1' = scaleFloat exy u1
                           v1' = scaleFloat exy (if u2 > 0 then nextUp u1 else nextDown u1)
                       in if u1' == v1' then
                            -- u1 is not on a midpoint, or u1' was rounded toward zero.
                            let u1'' = scaleFloat (-exy) u1' -- rounded value
                                u2'' = u1 - u1'' + u2
                            in (u1', 0 * (u1 - u1'' + u2)) -- signed zero
                          else
                            -- u1 or nextUp/nextDown u1 is on a midpoint
                            if isMantissaEven u1' then
                              (v1', 0 * (u1 - scaleFloat (-exy) v1' + u2))
                            else
                              (u1', 0 * (u1 - scaleFloat (-exy) u1' + u2))
  where
    d = floatDigits x
    (expMin,_expMax) = floatRange x

    -- Compute 'scaleFloat e z' with roundTiesTowardZero
    scaleFloatIntoSubnormalTiesTowardZero e z =
      let z' = scaleFloat e z
          w' = scaleFloat e (nextTowardZero z)
      in if z' == w' || not (isMantissaEven z) then
           z'
         else
           w'

augmentedAddition_viaRational :: forall a. (RealFloat a, Show a) => a -> a -> (a, a)
augmentedAddition_viaRational x y
  | isFinite x && isFinite y && (x /= 0 || y /= 0) =
    let z :: Rational
        z' :: a
        z = toRational x + toRational y
        z' = roundTiesTowardZero z
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w' :: a
             w = z - toRational z'
             w' = roundTiesTowardZero w
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x + y
                in (z, z)

augmentedMultiplication_viaRational :: forall a. (RealFloat a, Show a) => a -> a -> (a, a)
augmentedMultiplication_viaRational x y
  | isFinite x && isFinite y && x * y /= 0 =
    let z :: Rational
        z' :: a
        z = toRational x * toRational y
        z' = roundTiesTowardZero z
    in if isInfinite z' then
         (z', z')
       else
         let w :: Rational
             w' :: a
             w = z - toRational z'
             w' = roundTiesTowardZero w
         in if w == 0 then
              (z', 0 * z')
            else
              (z', w')
  | otherwise = let z = x * y
                in (z, z)

roundTiesTowardZero :: forall a. RealFloat a => Rational -> a
roundTiesTowardZero x | x < 0 = - fromPositiveRatio (- numerator x) (denominator x)
                      | otherwise = fromPositiveRatio (numerator x) (denominator x)
  where
    fromPositiveRatio :: Integer -> Integer -> a
    fromPositiveRatio !n !d
      = let ln, ld, e :: Int
            ln = integerLog2' n
            ld = integerLog2' d
            e = ln - ld - fDigits
            q, r, d_ :: Integer
            d_ | e >= 0 = d `unsafeShiftL` e
               | otherwise = d
            (!q, !r) | e >= 0 = n `quotRem` d_
                     | otherwise = (n `unsafeShiftL` (-e)) `quotRem` d
            !_ = assert (n % d * 2^^(-e) == fromInteger q + r % d_) ()
            -- e >= 0: n = q * (d * 2^e) + r, 0 <= r < d * 2^e
            -- e <= 0: n * 2^(-e) = q * d + r, 0 <= r < d
            -- n / d * 2^^(-e) = q + r / d_
            -- 52 <= log2 q < 54

            q', r', d' :: Integer
            e' :: Int
            (!q', !r', !d', !e') | q < (1 `unsafeShiftL` fDigits) = (q, r, d_, e)
                                 | otherwise = let (q'', r'') = q `quotRem` 2
                                               in (q'', r'' * d_ + r, 2 * d_, e + 1)
            !_ = assert (n % d * 2^^(-e') == fromInteger q' + r' % d') ()
            -- n / d * 2^^(-e') = q' + r' / d', 2^52 <= q' < 2^53, 0 <= r' < d'
            -- q' * 2^^e' <= n/d < (q'+1) * 2^^e', 2^52 <= q' < 2^53
            -- (q'/2^53) * 2^^(e'+53) <= n/d < (q'+1)/2^53 * 2^^(e'+53), 1/2 <= q'/2^53 < 1
            -- normal: 0x1p-1022 <= x <= 0x1.fffffffffffffp+1023
      in if expMin <= e' + fDigits && e' + fDigits <= expMax then
           -- normal
           if r' == 0 then
             encodeFloat q' e' -- exact
           else
             -- inexact
             let down = encodeFloat q' e'
                 up = encodeFloat (q' + 1) e' -- may be infinity
                 toNearest = case compare (2 * r') d' of
                   LT -> down
                   EQ -> down -- ties toward zero
                   GT -> up
             in toNearest
         else
           -- infinity or subnormal
           if expMax <= e' + fDigits then
             -- infinity
             (1 / 0) -- ToNearest
           else
             -- subnormal
             -- e' + fDigits < expMin (or, e' < expMin - fDigits = -1074)
             -- 0 <= rounded(n/d) <= 2^(expMin - 1) = 0x1p-1022, minimum (positive) subnormal: 0x1p-1074
             let (!q'', !r'') = q' `quotRem` (1 `unsafeShiftL` (expMin - fDigits - e'))
                 -- q' = q'' * 2^(expMin - fDigits - e') + r'', 0 <= r'' < 2^(expMin - fDigits - e')
                 -- 2^(fDigits-1) <= q' = q'' * 2^(expMin - fDigits - e') + r'' < 2^fDigits
                 -- n / d * 2^^(-e') = q' + r' / d' = q'' * 2^(expMin - fDigits - e') + r'' + r' / d'
                 -- n / d = q'' * 2^^(expMin - fDigits) + (r'' + r' / d') * 2^^e'
                 -- 0 <= r'' < 2^(expMin - fDigits - e')
             in if r' == 0 && r'' == 0 then
                  encodeFloat q'' (expMin - fDigits) -- exact
                else
                  let down = encodeFloat q'' (expMin - fDigits)
                      up = encodeFloat (q'' + 1) (expMin - fDigits)
                      toNearest = case compare r'' (1 `unsafeShiftL` (expMin - fDigits - e' - 1)) of
                                    LT -> down
                                    GT -> up
                                    EQ | r' /= 0   -> up
                                       | otherwise -> down -- ties toward zero
                  in toNearest
      where
        !fDigits = floatDigits (undefined :: a) -- 53 for Double
        (!expMin, !expMax) = floatRange (undefined :: a) -- (-1021, 1024) for Double


-- |
-- IEEE 754 @minimum@ operation.
-- @-0@ is smaller than @+0@.
-- Propagates NaNs.
minimum' :: RealFloat a => a -> a -> a
minimum' x y | x < y || isNaN x || (x == y && isNegativeZero x) = x
             | otherwise = y

-- |
-- IEEE 754 @minimumNumber@ operation.
-- @-0@ is smaller than @+0@.
-- Treats NaNs as missing data.
minimumNumber :: RealFloat a => a -> a -> a
minimumNumber x y | x < y || isNaN y || (x == y && isNegativeZero x) = x
                  | otherwise = y

-- |
-- IEEE 754 @maximum@ operation.
-- @-0@ is smaller than @+0@.
-- Propagates NaNs.
maximum' :: RealFloat a => a -> a -> a
maximum' x y | x < y || isNaN y || (x == y && isNegativeZero x) = y
             | otherwise = x

-- |
-- IEEE 754 @maximumNumber@ operation.
-- @-0@ is smaller than @+0@.
-- Treats NaNs as missing data.
maximumNumber :: RealFloat a => a -> a -> a
maximumNumber x y | x < y || isNaN x || (x == y && isNegativeZero x) = y
                  | otherwise = x

-- |
-- IEEE 754 @minimumMagnitude@ operation.
minimumMagnitude :: RealFloat a => a -> a -> a
minimumMagnitude x y | abs x < abs y = x
                     | abs y < abs x = y
                     | otherwise = minimum' x y

-- |
-- IEEE 754 @minimumMagnitudeNumber@ operation.
minimumMagnitudeNumber :: RealFloat a => a -> a -> a
minimumMagnitudeNumber x y | abs x < abs y = x
                           | abs y < abs x = y
                           | otherwise = minimumNumber x y

-- |
-- IEEE 754 @maximumMagnitude@ operation.
maximumMagnitude :: RealFloat a => a -> a -> a
maximumMagnitude x y | abs x > abs y = x
                     | abs y > abs x = y
                     | otherwise = maximum' x y

-- |
-- IEEE 754 @maximumMagnitudeNumber@ operation.
maximumMagnitudeNumber :: RealFloat a => a -> a -> a
maximumMagnitudeNumber x y | abs x > abs y = x
                           | abs y > abs x = y
                           | otherwise = maximumNumber x y
