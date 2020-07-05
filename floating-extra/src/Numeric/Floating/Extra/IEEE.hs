{-|
Module      : Numeric.Floating.Extra.IEEE
Description : IEEE 754-compliant operations for floating-point numbers

This module provides IEEE 754-compliant operations for floating-point numbers.

The functions in this module assume that the given floating-point type conform to IEEE 754 format.

Since 'RealFloat' constraint is insufficient to query properties of a NaN, the functions here assumes all NaN as positive, quiet.
If you want better treatment for NaNs, use the module "Numeric.Floating.Extra.IEEE.NaN".

Since floating-point exceptions cannot be accessed from Haskell in normal way, the operations provided by this module ignore exceptional behavior.
Don't let fp exceptions trap.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.Extra.IEEE
  (
  -- * 5.3 Homogeneous general-computational operations
  --
  -- ** 5.3.1 General operations
    roundToIntegralTiesToEven
  -- | @roundToIntegralTiesToAway@: not implemented yet
  , roundToIntegralTowardZero
  , roundToIntegralTowardPositive
  , roundToIntegralTowardNegative
  -- | @roundToIntegralExact@: not implemented yet
  , nextUp
  , nextDown
  -- | 'nextTowardZero' is not in IEEE, but may be useful to some.
  , nextTowardZero -- not in IEEE
  , remainder

  -- ** 5.3.2 Decimal operations (not supported)
  --
  -- | Not supported.

  -- ** 5.3.3 logBFormat operations (not supported)
  --
  -- | Not supported.

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
  , fusedMultiplyAdd_twoProduct
  , fusedMultiplyAddFloat_viaDouble
  , c_fusedMultiplyAddFloat
  , c_fusedMultiplyAddDouble
  -- |
  -- @convertFromInt@: not implemented yet
  , round    -- convertToIntegerTiesToEven: round
  , truncate -- convertToIntegerTowardZero: truncate
  , ceiling  -- convertToIntegerTowardPositive: ceiling
  , floor    -- convertToIntegerTowardNegative: floor
  -- |
  -- @convertToIntegerTiesToAway@: not implemented yet

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

  -- * Uncategorized
  , minPositive
  , maxFinite
  , distanceUlp
  , twoSum
  , twoProduct
  , split
  ) where
import           Data.Bits
import           Data.Ratio
import           GHC.Float (castDoubleToWord64, castFloatToWord32,
                            castWord32ToFloat, castWord64ToDouble,
                            double2Float, float2Double)
import           Numeric.Floating.Extra.Conversion
import Debug.Trace
import MyPrelude
import Control.Exception
import Numeric

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
                     in if m .&. m1 == 0
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
nextUpFloat x
  | not isFloatBinary32 = error "Numeric.Floating.Extra assumes Float is IEEE binary32"
  | isNaN x = x -- NaN -> itself
  | isNegativeZero x = encodeFloat 1 (expMin - d) -- -0 -> min positive
  | x < 0 = castWord32ToFloat (castFloatToWord32 x - 1) -- negative
  | otherwise = case castFloatToWord32 x of
                  0x7f80_0000 -> x -- positive infinity -> itself
                  w           -> castWord32ToFloat (w + 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
    -- Note: castFloatToWord32 is buggy on GHC <= 8.8 on x86_64, so we can't use it to test for NaN or negative number
    --   https://gitlab.haskell.org/ghc/ghc/issues/16617

-- |
-- prop> nextUpDouble 1 == 0x1.0000_0000_0000_1p0
-- prop> nextUpDouble (1/0) == 1/0
-- prop> nextUpDouble (-1/0) == - maxFinite
-- prop> nextUpDouble 0 == 0x1p-1074
-- prop> nextUpDouble (-0) == 0x1p-1074
-- prop> isNegativeZero (nextUpDouble (-0x1p-1074))
nextUpDouble :: Double -> Double
nextUpDouble x
  | not isDoubleBinary64 = error "Numeric.Floating.Extra assumes Double is IEEE binary64"
  | otherwise = case castDoubleToWord64 x of
                  w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
                    , w /= 0xfff0_0000_0000_0000 -> x -- NaN or positive infinity -> itself
                  0x8000_0000_0000_0000 -> encodeFloat 1 (expMin - d) -- -0 -> min positive
                  w | testBit w 63 -> castWord64ToDouble (w - 1) -- negative
                    | otherwise -> castWord64ToDouble (w + 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double

-- |
-- prop> nextDownFloat 1 == 0x1.fffffep-1
-- prop> nextDownFloat (1/0) == maxFinite
-- prop> nextDownFloat (-1/0) == -1/0
-- prop> nextDownFloat 0 == -0x1p-149
-- prop> nextDownFloat (-0) == -0x1p-149
-- prop> nextDownFloat 0x1p-149 == 0
nextDownFloat :: Float -> Float
nextDownFloat x
  | not isFloatBinary32 = error "Numeric.Floating.Extra assumes Float is IEEE binary32"
  | isNaN x || (isInfinite x && x < 0) = x -- NaN or negative infinity -> itself
  | isNegativeZero x || x < 0 = castWord32ToFloat (castFloatToWord32 x + 1) -- negative
  | x == 0 = encodeFloat (-1) (expMin - d) -- +0 -> max negative
  | otherwise = castWord32ToFloat (castFloatToWord32 x - 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
    -- Note: castFloatToWord32 is buggy on GHC <= 8.8 on x86_64, so we can't use it to test for NaN or negative number
    --   https://gitlab.haskell.org/ghc/ghc/issues/16617

-- |
-- prop> nextDownDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextDownDouble (1/0) == maxFinite
-- prop> nextDownDouble (-1/0) == -1/0
-- prop> nextDownDouble 0 == -0x1p-1074
-- prop> nextDownDouble (-0) == -0x1p-1074
-- prop> nextDownDouble 0x1p-1074 == 0
nextDownDouble :: Double -> Double
nextDownDouble x
  | not isDoubleBinary64  = error "Numeric.Floating.Extra assumes Double is IEEE binary64"
  | otherwise = case castDoubleToWord64 x of
                  w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
                    , w /= 0x7ff0_0000_0000_0000 -> x -- NaN or negative infinity -> itself
                  0x0000_0000_0000_0000 -> encodeFloat (-1) (expMin - d) -- +0 -> max negative
                  w | testBit w 63 -> castWord64ToDouble (w + 1) -- negative
                    | otherwise -> castWord64ToDouble (w - 1) -- positive
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double

-- |
-- prop> nextTowardZeroFloat 1 == 0x1.fffffep-1
-- prop> nextTowardZeroFloat (-1) == -0x1.fffffep-1
-- prop> nextTowardZeroFloat (1/0) == maxFinite
-- prop> nextTowardZeroFloat (-1/0) == -maxFinite
-- prop> nextTowardZeroFloat 0 == 0
-- prop> isNegativeZero (nextTowardZeroFloat (-0))
-- prop> nextTowardZeroFloat 0x1p-149 == 0
nextTowardZeroFloat :: Float -> Float
nextTowardZeroFloat x
  | not isFloatBinary32 = error "Numeric.Floating.Extra assumes Float is IEEE binary32"
  | isNaN x || x == 0 = x -- NaN or zero -> itself
  | otherwise = castWord32ToFloat (castFloatToWord32 x - 1) -- positive / negative
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double
    -- Note: castFloatToWord32 is buggy on GHC <= 8.8 on x86_64, so we can't use it to test for NaN or negative number
    --   https://gitlab.haskell.org/ghc/ghc/issues/16617

-- |
-- prop> nextTowardZeroDouble 1 == 0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (-1) == -0x1.ffff_ffff_ffff_fp-1
-- prop> nextTowardZeroDouble (1/0) == maxFinite
-- prop> nextTowardZeroDouble (-1/0) == -maxFinite
-- prop> nextTowardZeroDouble 0 == 0
-- prop> isNegativeZero (nextTowardZeroDouble (-0))
-- prop> nextTowardZeroDouble 0x1p-1074 == 0
nextTowardZeroDouble :: Double -> Double
nextTowardZeroDouble x
  | not isDoubleBinary64 = error "Numeric.Floating.Extra assumes Double is IEEE binary64"
  | otherwise = case castDoubleToWord64 x of
                  w | w .&. 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000
                    , w .&. 0x000f_ffff_ffff_ffff /= 0 -> x -- NaN -> itself
                  0x8000_0000_0000_0000 -> x -- -0 -> itself
                  0x0000_0000_0000_0000 -> x -- +0 -> itself
                  w -> castWord64ToDouble (w - 1) -- positive / negative
  where
    d, expMin :: Int
    d = floatDigits x -- 53 for Double
    (expMin,expMax) = floatRange x -- (-1021,1024) for Double

-- |
-- prop> \a b -> case twoSum a b of (x, y) -> a + b == x && toRational a + toRational b == toRational x + toRational y
twoSum :: Num a => a -> a -> (a, a)
twoSum a b =
  let x = a + b
      t = x - a
      y = (a - (x - t)) + (b - t)
  in (x, y)
{-# SPECIALIZE twoSum :: Float -> Float -> (Float, Float), Double -> Double -> (Double, Double) #-}

-- |
-- prop> \a b -> case twoProd a b of (x, y) -> a * b == x && fromRational (toRational a * toRational b - toRational x) == y
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
  | isInfinite c && isFinite a && isFinite b = c -- Returns infinity even if a * b overflows
  | isNaN a || isNaN b || isNaN c || isInfinite a || isInfinite b || isInfinite c = a * b + c
  | otherwise = let eab | a == 0 || b == 0 = fst (floatRange a) - floatDigits a -- reasonably small
                        | otherwise = exponent a + exponent b
                    ec | c == 0 = fst (floatRange c) - floatDigits c
                       | otherwise = exponent c
                    a' = significand a -- a' == a * radix^^(-exponent a)
                    b' = significand b -- b' == b * radix^^(-exponent b)
                    c' = significand c -- c' == c * radix^^(-ec)
                    (x', y') = twoProduct_nonscaling a' b'
                    e = max eab ec
                    x = scaleFloat (eab - e) x'
                    y = scaleFloat (eab - e) y'
                    d' = scaleFloat (ec - e) c'
                    d | y == scaleFloat (- floatDigits y') 0.5 = scaleFloat (max (2 - 2 * floatDigits c) (ec - e)) c'
                      | otherwise = scaleFloat (ec - e) c'
                    t = x + d
                    result0 | abs x > abs d = t + (y + ((x - t) + d))
                            | otherwise     = t + (y + ((d - t) + x))
                    -- result1 = t - result0
                    result = scaleFloat e result0
                in assert (toRational a' * toRational b' == toRational x' + toRational y') $
                   -- trace (showString "result0: " . showHFloat result0 . showString " expected: " . showHFloat (fromRational (toRational x + toRational y + toRational d') `asTypeOf` result0) . showString " x: " . showHFloat x . showString " y: " . showHFloat y . showString " d: " . showHFloat d . showString " d': " . showHFloat d' . showString " t: " . showHFloat t . showString " ec: " . shows ec . showString " e: " . shows e $ "") $
                   -- assert (toRational result0 + toRational result1 == (toRational x'' + toRational y'' + toRational c'')) $
                   assert (result0 == fromRational (toRational x + toRational y + toRational d')) $
                   if result0 == 0 then
                     -- We need to handle the sign of zero
                     if c == 0 && a /= 0 && b /= 0 then
                       a * b -- let the product underflow
                     else
                       a * b + c -- -0 if both a * b and c are -0
                   else
                     if isDenormalized result then
                       -- The rounding in 'scaleFloat e result0' might have yielded an incorrect result.
                       -- Take the slow path.
                       case toRational a * toRational b + toRational c of
                         0 -> a * b + c
                         r -> fromRational r
                     else
                       result
-- {-# SPECIALIZE fusedMultiplyAdd_twoProduct :: Float -> Float -> Float -> Float, Double -> Double -> Double -> Double #-}
{-# NOINLINE fusedMultiplyAdd_twoProduct #-}

-- TODO: Check if this works correctly
fusedMultiplyAddFloat_viaDouble :: Float -> Float -> Float -> Float
fusedMultiplyAddFloat_viaDouble a b c
  | isFloatBinary32 && isDoubleBinary64 = realFloatToFrac (realFloatToFrac a * realFloatToFrac b + realFloatToFrac c :: Double)
  | otherwise = error "fusedMultiplyAdd/Float: unexpected configuration"

-- |
-- IEEE 754 @fusedMultiplyAdd@ operation.
fusedMultiplyAdd :: RealFloat a => a -> a -> a -> a
fusedMultiplyAdd x y z
  | isFinite x && isFinite y = if isFinite z then
                                 case toRational x * toRational y + toRational z of
                                   0 -> x * y + z
                                   r -> fromRational r
                               else
                                 if isInfinite z then
                                   z -- x * y is finite
                                 else
                                   x * y + z -- z is NaN
  | otherwise = x * y + z -- either x or y is Infinity or NaN
{-# NOINLINE [1] fusedMultiplyAdd #-}

-- #ifdef USE_FFI

foreign import ccall unsafe "fmaf"
  c_fusedMultiplyAddFloat :: Float -> Float -> Float -> Float
foreign import ccall unsafe "fma"
  c_fusedMultiplyAddDouble :: Double -> Double -> Double -> Double

{-
{-# RULES
"fusedMultiplyAdd/Float" fusedMultiplyAdd = c_fusedMultiplyAddFloat
"fusedMultiplyAdd/Double" fusedMultiplyAdd = c_fusedMultiplyAddDouble
  #-}
-}

-- #else

-- TODO: implement Float version of FMA with Double

-- #endif

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
remainder x y = let n = round (toRational x / toRational y)
                in x - y * fromInteger n

-- |
-- IEEE 754 @roundToIntegralTiesToEven@ operation.
--
-- prop> \x -> roundToIntegralTiesToEven x == fromInteger (round x)
roundToIntegralTiesToEven :: RealFloat a => a -> a
roundToIntegralTiesToEven x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTiesToEven x = case round x of
                                0 | x < 0 -> -0
                                  | otherwise -> 0
                                n -> fromInteger n

-- |
-- IEEE 754 @roundToIntegralTowardZero@ operation.
--
-- prop> \x -> roundToIntegralTowardZero x == fromInteger (truncate x)
roundToIntegralTowardZero :: RealFloat a => a -> a
roundToIntegralTowardZero x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTowardZero x = case truncate x of
                                0 | x < 0 -> -0
                                  | otherwise -> 0
                                n -> fromInteger n

-- |
-- IEEE 754 @roundToIntegralTowardPositive@ operation.
--
-- prop> \x -> roundToIntegralTowardPositive x == fromInteger (ceiling x)
roundToIntegralTowardPositive :: RealFloat a => a -> a
roundToIntegralTowardPositive x | isInfinite x || isNaN x || isNegativeZero x = x
roundToIntegralTowardPositive x = case ceiling x of
                                    0 | x < 0 -> -0
                                      | otherwise -> 0
                                    n -> fromInteger n

-- |
-- IEEE 754 @roundToIntegralTowardNegative@ operation.
--
-- prop> \x -> roundToIntegralTowardNegative x == fromInteger (floor x)
roundToIntegralTowardNegative :: RealFloat a => a -> a
roundToIntegralTowardNegative x | isInfinite x || isNaN x || isNegativeZero x = x
                                | otherwise = fromInteger (floor x)

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
-- TODO: Specialize for Float, Double
{-# NOINLINE [1] isNormal #-}
{-# RULES
"isNormal/Double" isNormal = isDoubleNormal
  #-}

isDoubleNormal :: Double -> Bool
isDoubleNormal x = let w = castDoubleToWord64 x .&. 0x7ff0_0000_0000_0000
                   in w /= 0 && w /= 0x7ff0_0000_0000_0000

-- |
-- IEEE 754 @isFinite@ operation.
isFinite :: RealFloat a => a -> Bool
isFinite x = not (isNaN x) && not (isInfinite x)
-- TODO: Specialize for Float, Double

-- There's isDoubleFinite in GHC.Float

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
-- TODO: Specialize for Float, Double

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
               EQ -- TODO
  | otherwise = case (isNaN x, isNaN y) of
                  (True, False) -> GT -- TODO: sign bit of NaN
                  (False, True) -> LT -- TODO: sign bit of NaN
                  _ -> EQ -- must be (True, True). TODO: sign bit of NaN, payload of NaN
-- TODO: Specialize for Float, Double

-- |
-- Comparison with IEEE 754 @totalOrderMag@ predicate.
compareByTotalOrderMag :: RealFloat a => a -> a -> Ordering
compareByTotalOrderMag x y = compareByTotalOrder (abs x) (abs y)

-- isCanonical :: a -> Bool

-- data Ordering4 = LT | EQ | GT | UNORD

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
                        (True, 0, 0) -> NegativeZero
                        (False, 0, 0) -> PositiveZero
                        (True, 0, _) -> NegativeSubnormal
                        (False, 0, _) -> PositiveSubnormal
                        (True, 0x7ff, 0) -> NegativeInfinity
                        (False, 0x7ff, 0) -> PositiveInfinity
                        (_, 0x7ff, _) -> QuietNaN -- ignore signaling NaN
                        (True, _, _) -> NegativeNormal
                        (False, _, _) -> NegativeNormal

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
