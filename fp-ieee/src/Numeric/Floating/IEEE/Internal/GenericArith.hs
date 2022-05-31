{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Floating.IEEE.Internal.GenericArith where
import           Data.Proxy
import           MyPrelude
import           Numeric.Floating.IEEE.Internal.Classify
import           Numeric.Floating.IEEE.Internal.Conversion
import           Numeric.Floating.IEEE.Internal.FMA

default ()

-- $setup
-- >>> :m + Data.Proxy
-- >>> import Numeric.Floating.IEEE.Internal.GenericArith

infixl 6 `genericAdd`, `genericSub`
infixl 7 `genericMul`, `genericDiv`

-- |
-- IEEE 754 @addition@ operation.
genericAdd :: (RealFloat a, RealFloat b) => a -> a -> b
genericAdd x y | x == 0 && y == 0 = realFloatToFrac (x + y)
               | isFinite x && isFinite y = fromRational (toRational x + toRational y)
               | otherwise = realFloatToFrac (x + y)
{-# NOINLINE [1] genericAdd #-}

-- |
-- IEEE 754 @subtraction@ operation.
genericSub :: (RealFloat a, RealFloat b) => a -> a -> b
genericSub x y | x == 0 && y == 0 = realFloatToFrac (x - y)
               | isFinite x && isFinite y = fromRational (toRational x - toRational y)
               | otherwise = realFloatToFrac (x - y)
{-# NOINLINE [1] genericSub #-}

-- |
-- IEEE 754 @multiplication@ operation.
genericMul :: (RealFloat a, RealFloat b) => a -> a -> b
genericMul x y | x == 0 || y == 0 = realFloatToFrac (x * y)
               | isFinite x && isFinite y = fromRational (toRational x * toRational y)
               | otherwise = realFloatToFrac (x * y)
{-# NOINLINE [1] genericMul #-}

-- |
-- IEEE 754 @division@ operation.
genericDiv :: (RealFloat a, RealFloat b) => a -> a -> b
genericDiv x y | x == 0 || y == 0 = realFloatToFrac (x / y)
               | isFinite x && isFinite y = fromRational (toRational x / toRational y)
               | otherwise = realFloatToFrac (x / y)
{-# NOINLINE [1] genericDiv #-}

{-
-- |
-- IEEE 754 @squareRoot@ operation.
genericSqrt :: (RealFloat a, RealFloat b) => a -> b
genericSqrt x | x == 0 = realFloatToFrac x
              | x > 0, isFinite x = error "not implemented yet"
              | otherwise = realFloatToFrac (sqrt x)
-}

-- |
-- IEEE 754 @fusedMultiplyAdd@ operation.
genericFusedMultiplyAdd :: (RealFloat a, RealFloat b) => a -> a -> a -> b
genericFusedMultiplyAdd a b c
  | isFinite a && isFinite b && isFinite c = case toRational a * toRational b + toRational c of
                                               0 | isNegativeZero (a * b + c) -> -0
                                               r -> fromRational r
  | isFinite a && isFinite b = realFloatToFrac c -- c is Infinity or NaN
  | otherwise = realFloatToFrac (a * b + c)
{-# NOINLINE [1] genericFusedMultiplyAdd #-}

{-# RULES
"genericAdd/a->a" genericAdd = (+)
"genericSub/a->a" genericSub = (-)
"genericMul/a->a" genericMul = (*)
"genericDiv/a->a" genericDiv = (/)
"genericFusedMultiplyAdd/a->a" genericFusedMultiplyAdd = fusedMultiplyAdd
  #-}

-- | Returns True if @a@ is a subtype of @b@
--
-- >>> isSubFloatingType (undefined :: Float) (undefined :: Double)
-- True
-- >>> isSubFloatingType (undefined :: Double) (undefined :: Float)
-- False
-- >>> isSubFloatingType (undefined :: Double) (undefined :: Double)
-- True
isSubFloatingType :: (RealFloat a, RealFloat b) => a -> b -> Bool
isSubFloatingType a b = ieeeA && ieeeB && baseA == baseB && eminB <= eminA && emaxA <= emaxB && digitsA <= digitsB
  where
    ieeeA = isIEEE a
    ieeeB = isIEEE b
    baseA = floatRadix a
    baseB = floatRadix b
    (eminA,emaxA) = floatRange a
    (eminB,emaxB) = floatRange b
    digitsA = floatDigits a
    digitsB = floatDigits b

-- | Returns True if @a@ is a subtype of @b@
--
-- >>> isSubFloatingTypeProxy (Proxy :: Proxy Float) (Proxy :: Proxy Double)
-- True
-- >>> isSubFloatingTypeProxy (Proxy :: Proxy Double) (Proxy :: Proxy Float)
-- False
-- >>> isSubFloatingTypeProxy (Proxy :: Proxy Double) (Proxy :: Proxy Double)
-- True
isSubFloatingTypeProxy :: (RealFloat a, RealFloat b) => Proxy a -> Proxy b -> Bool
isSubFloatingTypeProxy proxyA proxyB = isSubFloatingType (undefined `asProxyTypeOf` proxyA) (undefined `asProxyTypeOf` proxyB)
