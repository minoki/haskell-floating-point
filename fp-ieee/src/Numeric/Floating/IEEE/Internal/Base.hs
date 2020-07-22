{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.Base
  ( isFloatBinary32
  , isDoubleBinary64
  , minPositive
  , minPositiveNormal
  , maxFinite
  , (^!)
  , negateIntAsWord
  , absIntAsWord
  ) where
import           Data.Bits
import           MyPrelude

default ()

-- $setup
-- >>> :set -XHexFloatLiterals -XNumericUnderscores
-- >>> import Numeric.Floating.IEEE.Internal.NextFloat (nextDown)

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

-- |
-- prop> (minPositiveNormal :: Float) == 0x1p-126
-- prop> (minPositiveNormal :: Double) == 0x1p-1022
-- prop> not (isDenormalized (minPositiveNormal :: Float))
-- prop> not (isDenormalized (minPositiveNormal :: Double))
-- prop> isDenormalized (nextDown (minPositiveNormal :: Float))
-- prop> isDenormalized (nextDown (minPositiveNormal :: Double))
minPositiveNormal :: RealFloat a => a
minPositiveNormal = let (expMin,_expMax) = floatRange x
                        x = encodeFloat 1 (expMin - 1)
                    in x
{-# INLINABLE minPositiveNormal #-}
{-# SPECIALIZE minPositiveNormal :: Float, Double #-}

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

-- A variant of (^) that allows constant folding
infixr 8 ^!
(^!) :: Integer -> Int -> Integer
(^!) = (^)
{-# INLINE [0] (^!) #-}

pow_helper :: Bool -> Integer -> Int -> Integer
pow_helper _ x y = x ^ y
{-# INLINE [0] pow_helper #-}
{-# RULES
"x^!" forall x y. x ^! y = pow_helper (y > 0) x y
"pow_helper" forall x y.
  pow_helper True x y = if y `rem` 2 == 0 then
                          (x * x) ^! (y `quot` 2)
                        else
                          x * (x * x) ^! (y `quot` 2)
  #-}

-- |
-- prop> negateIntAsWord minBound == fromInteger (negate (fromIntegral (minBound :: Int)))
negateIntAsWord :: Int -> Word
negateIntAsWord x = fromIntegral (negate x)

-- |
-- prop> absIntAsWord minBound == fromInteger (abs (fromIntegral (minBound :: Int)))
absIntAsWord :: Int -> Word
absIntAsWord x = fromIntegral (abs x)

{- More careful definitions:

negateIntAsWord :: Int -> Word
negateIntAsWord x | x == minBound = fromInteger (negate (fromIntegral (minBound :: Int)))
                  | otherwise = fromIntegral (negate x)

absIntAsWord :: Int -> Word
absIntAsWord x | x == minBound = fromInteger (abs (fromIntegral (minBound :: Int)))
               | otherwise = fromIntegral (abs x)
-}
