{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Decimal.IEEE where
import           Control.Exception (assert)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Ratio ((%))
import           GHC.TypeNats
import           Math.NumberTheory.Logarithms (naturalLog10)
import           Numeric.Floating.IEEE hiding (exponent)
import qualified Numeric.Floating.IEEE.NaN as IEEE.NaN
import           Numeric.Natural
import           Prelude hiding (exponent, significand)
import qualified Prelude
import           Text.Show (showParen, showString)

data Decimal (emax :: Nat) (p :: Nat)
  -- @significand * 10^^(exponent-(p-1))@
  = Finite { sign        :: !Bool -- ^ @True@ if negative
           , exponent    :: !Int -- ^ @emin <= e <= emax@
           , significand :: !Natural -- ^ @0 <= significand < 10^p@
           }
  | Infinity { sign :: !Bool -- ^ @True@ if negative
             }
  | NaN { sign        :: !Bool -- ^ @True@ if negative
        , isSignaling :: !Bool -- ^ @True@ if signaling
        , payload     :: !Natural
        }

isValidDecimal :: forall emax p. (KnownNat emax, KnownNat p) => Decimal emax p -> Bool
isValidDecimal (Finite { exponent = e, significand = n }) = emin <= e && e <= emax && n < 10^p {- or, (n == 0 || naturalLog10 n < p) -}
  where
    p = fromIntegral (natVal (Proxy :: Proxy p))
    emax = fromIntegral (natVal (Proxy :: Proxy emax))
    emin = 1 - emax

isValidDecimal (Infinity {}) = True
isValidDecimal (NaN { payload = _ }) = True -- TODO: range of payload?

defaultNaN :: Decimal emax p
defaultNaN = NaN { sign = False, isSignaling = False, payload = 0 }

quieten :: Decimal emax p -> Decimal emax p
quieten (NaN s True p) = NaN s False p
quieten x              = x

negateIf :: Num a => Bool -> a -> a
negateIf True x  = negate x
negateIf False x = x

reverseIf :: Bool -> Ordering -> Ordering
reverseIf True x = compare EQ x
reverseIf False x = x

instance (KnownNat emax, KnownNat p) => Eq (Decimal emax p) where
  NaN {} == _ = False
  _ == NaN {} = False
  Infinity s1 == Infinity s2 = s1 == s2
  Finite { sign = s1, exponent = e1, significand = n1 } == Finite { sign = s2, exponent = e2, significand = n2 }
    | n1 == 0 && n2 == 0 = True
    | n1 == 0 || n2 == 0 = False
    | s1 /= s2 = False
    | e1 - e2 == 0 = n1 == n2
    | e1 - e2 > 0 = n1 * 10^(e1-e2) == n2
    | otherwise {- e2 - e1 > 0 -} = n1 == n2 * 10^(e2-e1)
  _ == _ = False

partialCompare :: Decimal emax p -> Decimal emax p -> Maybe Ordering
partialCompare (NaN {}) _ = Nothing
partialCompare _ (NaN {}) = Nothing
partialCompare (Infinity { sign = s1 }) (Infinity { sign = s2 }) = Just (compare s2 s1)
partialCompare (Infinity { sign = s1 }) (Finite {}) | s1 = Just LT
                                                    | otherwise = Just GT
partialCompare (Finite {}) (Infinity { sign = s2 }) | s2 = Just GT
                                                    | otherwise = Just LT
partialCompare (Finite { sign = s1, exponent = e1, significand = n1 }) (Finite { sign = s2, exponent = e2, significand = n2 })
  | n1 == 0 && n2 == 0 = Just EQ
  | n1 == 0 = Just $ if s2 then GT else LT
  | n2 == 0 = Just $ if s1 then LT else GT
  | s1 /= s2 = Just $ compare s2 s1
  | e1 == e2 = Just $ reverseIf s1 $ compare n1 n2
  | e1 - e2 > 0 = Just $ reverseIf s1 $ compare (n1 * 10^(e1-e2)) n2
  | otherwise {- e2 - e1 > 0 -} = Just $ reverseIf s1 $ compare n1 (n2 * 10^(e2-e1))

instance (KnownNat emax, KnownNat p) => Ord (Decimal emax p) where
  compare x y = fromMaybe GT $ partialCompare x y
  x <  y = maybe False (== LT) $ partialCompare x y
  x <= y = maybe False (/= GT) $ partialCompare x y
  x >  y = maybe False (== GT) $ partialCompare x y
  x >= y = maybe False (/= LT) $ partialCompare x y

showPositiveDecimal :: Int -> Natural -> ShowS
showPositiveDecimal e 0 = showString "0e" . shows e
showPositiveDecimal e n = let k = naturalLog10 n
                              s = show n
                              -- 10^k <= n < 10^(k+1)
                              -- length s == k+1
                          in if k == 0 then
                               -- length s == 1
                               showString s . showChar 'e' . shows e
                             else
                               showString (head s : '.' : tail s) . showChar 'e' . shows (e - k)

instance (KnownNat emax, KnownNat p) => Show (Decimal emax p) where
  showsPrec prec (Finite { sign = s, exponent = e, significand = n })
    | s = showParen (prec > 6) (showChar '-' . showPositiveDecimal e n)
    | otherwise = showPositiveDecimal e n
  showsPrec prec (Infinity { sign = s })
    | s = showParen (prec > 6) (showString "-Infinity")
    | otherwise = showString "Infinity"
  showsPrec _ (NaN {}) = showString "NaN"

-- n * 10^^(e-(p-1))
makeDecimal :: forall emax p. (KnownNat emax, KnownNat p) => Bool -> Int -> Natural -> Decimal emax p
makeDecimal neg e 0 = Finite { sign = neg, exponent = min emax (max emin e), significand = 0 }
  where
    emax, emin :: Int
    emax = fromIntegral (natVal (Proxy :: Proxy emax))
    emin = 1 - emax
makeDecimal neg e n = let k = naturalLog10 n
                          -- 10^k <= n < 10^(k+1)
                          -- 10^(k+e) <= n * 10^e < 10^(k+e+1)
                      in if emin + (p - 1) <= k + e && k + e <= emax + (p - 1) then
                           -- normal
                           if k < p then
                             -- emin <= emin + (p - 1) - k <= e <= emax + (p - 1) - k <= emax
                             Finite { sign = neg, exponent = e, significand = n }
                           else
                             -- p <= k
                             let (q,r) = n `quotRem` (10^(k-p+1))
                                 q' = case compare r (10^(k-p)) of
                                        LT -> q
                                        EQ | even q -> q
                                           | otherwise -> q + 1
                                        GT -> q + 1
                                 !_ = assert (10^(p-1) <= q' && q' <= 10^p) ()
                                 !_ = assert (emin <= e + k - p + 1 && e + k - p + 1 <= emax) ()
                             in internalEncodeDecimal q' (e + k - p + 1)
                         else
                           if emax + (p - 1) < k + e then
                             -- overflow
                             Infinity { sign = neg }
                           else
                             -- underflow: k + e < emin + (p - 1)
                             if emin <= e then
                               -- emin <= e < emin + (p - 1) - k => 0 < (p - 1) - k => k < (p - 1)
                               -- n < 10^(p-1)
                               let !_ = assert (n < 10^(p-1)) ()
                               in Finite { sign = neg, exponent = e, significand = n }
                             else
                               let (q,r) = n `quotRem` (10^(emin + 1 - e))
                                   q' = case compare r (10^(emin - e)) of
                                          LT -> q
                                          EQ | even q -> q
                                             | otherwise -> q + 1
                                          GT -> q + 1
                                   !_ = assert (10^(p-1) <= q' && q' <= 10^p) ()
                               in internalEncodeDecimal q' emin

  where
    p, emax, emin :: Int
    p = fromIntegral (natVal (Proxy :: Proxy p))
    emax = fromIntegral (natVal (Proxy :: Proxy emax))
    emin = 1 - emax

    internalEncodeDecimal :: Natural -> Int -> Decimal emax p
    internalEncodeDecimal n e | assert (n <= 10^p && emin <= e && e <= emax) False = undefined
                              | n < 10^p = Finite { sign = neg, exponent = e, significand = n }
                              | e < emax = Finite { sign = neg, exponent = e + 1, significand = n `quot` 10 }
                              | otherwise = Infinity { sign = neg }

makeDecimalI :: (KnownNat emax, KnownNat p) => Bool -> Int -> Integer -> Decimal emax p
makeDecimalI signOfZero e n | n == 0 = makeDecimal signOfZero e 0
                            | n < 0 = makeDecimal True e (fromInteger (- n))
                            | otherwise {- n > 0 -} = makeDecimal False e (fromInteger n)

instance (KnownNat emax, KnownNat p) => Num (Decimal emax p) where
  x@NaN {} + _ = quieten x
  _ + x@NaN {} = quieten x
  x@(Infinity s1) + Infinity s2 | s1 == s2 = x
                                | otherwise = defaultNaN
  x@Infinity {} + Finite {} = x
  Finite {} + x@Infinity {} = x
  Finite { sign = s1, exponent = e1, significand = n1 } + Finite { sign = s2, exponent = e2, significand = n2 }
    = let e = min e1 e2
          x1 = negateIf s1 (fromIntegral n1 * 10^(e1-e))
          x2 = negateIf s2 (fromIntegral n2 * 10^(e2-e))
      in makeDecimalI (s1 && s2) e (x1 + x2)

  x@NaN {} - _ = quieten x
  _ - x@NaN {} = quieten x
  x@(Infinity s1) - Infinity s2 | s1 /= s2 = x
                                | otherwise = defaultNaN
  x@Infinity {} - Finite {} = x
  Finite {} - Infinity s = Infinity (not s)
  Finite { sign = s1, exponent = e1, significand = n1 } - Finite { sign = s2, exponent = e2, significand = n2 }
    = let e = min e1 e2
          x1 = negateIf s1 (fromIntegral n1 * 10^(e1-e))
          x2 = negateIf s2 (fromIntegral n2 * 10^(e2-e))
      in makeDecimalI (s1 && not s2) e (x1 - x2)

  x@NaN {} * _ = quieten x
  _ * x@NaN {} = quieten x
  Infinity s1 * Infinity s2 = Infinity (s1 /= s2)
  Infinity s1 * Finite { sign = s2, significand = n } | n == 0 = defaultNaN { sign = s1 /= s2 }
                                                      | otherwise = Infinity { sign = s1 /= s2 }
  Finite { sign = s1, significand = n } * Infinity s2 | n == 0 = defaultNaN { sign = s1 /= s2 }
                                                      | otherwise = Infinity { sign = s1 /= s2 }
  Finite { sign = s1, exponent = e1, significand = n1 } * Finite { sign = s2, exponent = e2, significand = n2 } = makeDecimal (s1 /= s2) (e1 + e2) (n1 * n2)

  negate x = x { sign = not (sign x) }
  abs x = x { sign = False }
  signum x | x < 0 = -1
           | x > 0 = 1
           | otherwise = x
  fromInteger n = makeDecimalI False 0 n

instance (KnownNat emax, KnownNat p) => Fractional (Decimal emax p) where
  x@NaN {} / _ = quieten x
  _ / x@NaN {} = quieten x
  Infinity s1 / Infinity s2 = defaultNaN { sign = s1 /= s2 }
  Infinity s1 / Finite { sign = s2 } = Infinity (s1 /= s2)
  Finite { sign = s1 } / Infinity s2 = Finite { sign = s1 /= s2, exponent = emin, significand = 0 }
    where
      emax, emin :: Int
      emax = fromIntegral (natVal (Proxy :: Proxy emax))
      emin = 1 - emax
  Finite { sign = s1, significand = n1 } / Finite { sign = s2, significand = 0 }
    | n1 == 0 = defaultNaN { sign = s1 /= s2 }
    | otherwise = Infinity { sign = s1 /= s2 }
  Finite { sign = s1, exponent = e1, significand = 0 } / Finite { sign = s2, exponent = e2 } = Finite { sign = s1 /= s2, exponent = max emin (min emax (e1 - e2)), significand = 0 }
    where
      emax = fromIntegral (natVal (Proxy :: Proxy emax))
      emin = 1 - emax
  Finite { sign = s1, exponent = e1, significand = n1 } / Finite { sign = s2, exponent = e2, significand = n2 } = (scaleFloat (e1 - e2) (fromRational (fromIntegral n1 % fromIntegral n2) :: Decimal emax p)) { sign = s1 /= s2 } -- TODO
  fromRational = fromRationalTiesToEven

liftUnary :: (KnownNat emax, KnownNat p) => (Double -> Double) -> Decimal emax p -> Decimal emax p
liftUnary f x = realFloatToFrac (f (realFloatToFrac x))

instance (KnownNat emax, KnownNat p) => Floating (Decimal emax p) where
  pi = 3.14159265358979323842264338327950 -- TODO
  exp = liftUnary exp
  log = liftUnary log
  sqrt = liftUnary sqrt -- TODO
  sin = liftUnary sin
  cos = liftUnary cos
  asin = liftUnary asin
  acos = liftUnary acos
  atan = liftUnary atan
  sinh = liftUnary sinh
  cosh = liftUnary cosh
  asinh = liftUnary asinh
  acosh = liftUnary acosh
  atanh = liftUnary atanh

instance (KnownNat emax, KnownNat p) => Real (Decimal emax p) where
  toRational (Finite { sign = s, exponent = e, significand = n }) = negateIf s (fromIntegral n * 10^^(e - (p - 1)))
    where
      p = fromIntegral (natVal (Proxy :: Proxy p))
  toRational (Infinity {}) = error "toRational: infinity"
  toRational (NaN {}) = error "toRational: NaN"

instance (KnownNat emax, KnownNat p) => RealFrac (Decimal emax p) where
  properFraction x@(Finite { sign = s, exponent = e, significand = n }) = case properFraction (toRational x) of
                                                                            (n, t) -> (n, fromRational t) -- TODO: better implementation
  properFraction (Infinity {}) = error "properFraction: infinity"
  properFraction (NaN {}) = error "properFraction: NaN"

instance (KnownNat emax, KnownNat p) => RealFloat (Decimal emax p) where
  floatRadix _ = 10
  floatDigits _ = fromIntegral (natVal (Proxy :: Proxy p))
  floatRange _ = let emax = fromIntegral (natVal (Proxy :: Proxy emax))
                     emin = 1 - emax
                 in (1 + emin, 1 + emax)

  decodeFloat (Finite { significand = 0 }) = (0, 0)
  decodeFloat (Finite { sign = s, exponent = e, significand = n })
    = let p = fromIntegral (natVal (Proxy :: Proxy p))
          k = naturalLog10 n
          !_ = assert (0 <= k && k <= p-1) ()
          -- log10 m = log10 n + (p-1-k)
          --         = p - 1 + (log10 n - floor (log10 n))
          -- p - 1 <= log10 m < p
          -- 10^(p-1) <= m < 10^p
          m = fromIntegral n * 10^(p-1-k)
          -- abs x = m * 10^f
          -- n * 10^^(e-(p-1)) = m * 10^f
          --                   = n * 10^(p-1-k) * 10^f
          -- e-(p-1) = p-1-k + f
          -- f = e + k - 2*(p-1)
          f = e - (p-1) - ((p-1) - k)
      in (negateIf s m, f)
  decodeFloat (Infinity {}) = error "decodeFloat: infinity"
  decodeFloat (NaN {}) = error "decodeFloat: NaN"

  encodeFloat 0 f = Finite { sign = False, exponent = f, significand = 0 }
  encodeFloat m f | 10^p < abs m = error "encodeFloat: invalid input"
                  | 10^p == abs m = let m' = m `quot` 10
                                        f' = f + 1
                                    in if emax < f' then
                                         Infinity { sign = m < 0 }
                                       else
                                         doEncodeFloat m' f'
                  | otherwise = doEncodeFloat m f
    where
      p = fromIntegral (natVal (Proxy :: Proxy p))
      emax = fromIntegral (natVal (Proxy :: Proxy emax))
      emin = 1 - emax
      doEncodeFloat m f | emin <= f && f <= emax = Finite { sign = m < 0, exponent = f, significand = fromInteger (abs m) }
                        | f < emin = case m `quotRem` (10^(emin - f)) of
                                       (q, 0) -> Finite { sign = m < 0, exponent = emin, significand = fromInteger (abs q) }
                                       _ -> error "encodeFloat: inexact"
                        | otherwise = error "encodeFloat: overflow"

  exponent (Finite { significand = 0 }) = 0
  exponent (Finite { exponent = e, significand = n }) = let k = naturalLog10 n
                                                        in e + k + 2 - p
    where
      p = fromIntegral (natVal (Proxy :: Proxy p))
  exponent (Infinity {}) = error "exponent: infinity"
  exponent (NaN {}) = error "exponent: NaN"

  significand x@(Finite { significand = 0 }) = x { sign = False }
  significand (Finite { sign = s, exponent = e, significand = n }) = Finite { sign = s, exponent = e - t, significand = n }
    where
      p = fromIntegral (natVal (Proxy :: Proxy p))
      -- 10^(-1) <= n * 10^(e-(p-1)) / 10^t < 1
      -- -1 <= log10 n + e - (p-1) - t < 0
      -- t <= e + log10 n - (p-1) + 1
      t = e + naturalLog10 n - p + 2
  significand (Infinity {}) = error "significand: infinity"
  significand (NaN {}) = error "significand: NaN"

  scaleFloat t (Finite { sign = s, exponent = e, significand = n })
    | e + t < emin = makeDecimal s (e + t) n
    | emax < e + t = Infinity { sign = s }
    | otherwise {- emin <= e + t && e + t <= emax -} = Finite { sign = s, exponent = e + t, significand = n }
    where
      emax = fromIntegral (natVal (Proxy :: Proxy emax))
      emin = 1 - emax
  scaleFloat _ x = quieten x -- infinity or NaN

  isNaN (NaN {}) = True
  isNaN _        = False

  isInfinite (Infinity {}) = True
  isInfinite _             = False

  isDenormalized (Finite { exponent = e, significand = 0 }) = False
  isDenormalized (Finite { exponent = e, significand = n }) = -- Precondition: emin <= e <= emax, 0 <= log10 n
                                                              -- The condition: 10^^emin <= n * 10^^(e-(p-1))
                                                              --            <=> 10^^(emin+(p-1)) <= n * 10^^e
                                                              --            <=> emin+(p-1) <= log10 n + e
                                                              emin + (p-1) <= naturalLog10 n + e
    where
      p = fromIntegral (natVal (Proxy :: Proxy p))
      emax = fromIntegral (natVal (Proxy :: Proxy emax))
      emin = 1 - emax
  isDenormalized (Infinity {}) = False
  isDenormalized (NaN {}) = False

  isNegativeZero (Finite { sign = True, significand = 0 }) = True
  isNegativeZero _                                         = False

  isIEEE _ = True

  -- atan2: use default

instance (KnownNat emax, KnownNat p) => IEEE.NaN.RealFloatNaN (Decimal emax p) where
  copySign x y = x { sign = IEEE.NaN.isSignMinus y }

  isSignMinus (Finite { sign = s })   = s
  isSignMinus (Infinity { sign = s }) = s
  isSignMinus (NaN { sign = s })      = s

  isSignaling (NaN { isSignaling = True }) = True
  isSignaling _                            = False

  getPayload (NaN { payload = a }) = fromIntegral a
  getPayload _                     = -1

  setPayload x | x < 0 = 0
               | otherwise = NaN { sign = False, isSignaling = False, payload = truncate x }

  setPayloadSignaling x | x < 0 = 0
                        | otherwise = NaN { sign = False, isSignaling = True, payload = truncate x }

  classify x@(Finite { sign = s, exponent = e, significand = n }) | n == 0 = if s then NegativeZero else PositiveZero
                                                                  | isDenormalized x = if s then NegativeSubnormal else PositiveSubnormal
                                                                  | otherwise = if s then NegativeNormal else PositiveNormal
  classify (Infinity { sign = s }) = if s then NegativeInfinity else PositiveInfinity
  classify (NaN { isSignaling = sn }) = if sn then SignalingNaN else QuietNaN

  equalByTotalOrder (NaN { sign = s1, isSignaling = sn1, payload = a1 }) (NaN { sign = s2, isSignaling = sn2, payload = a2 }) = s1 == s2 && sn1 == sn2 && a1 == a2
  equalByTotalOrder (Infinity { sign = s1 }) (Infinity { sign = s2 }) = s1 == s2
  equalByTotalOrder (Finite { sign = s1, exponent = e1, significand = n1 }) (Finite { sign = s2, exponent = e2, significand = n2 }) = s1 == s2 && e1 == e2 && n1 == n2
  equalByTotalOrder _ _ = False

  compareByTotalOrder x y = case partialCompare x y of
    Nothing -> case (x, y) of
                 (NaN { sign = s1, isSignaling = sn1, payload = a1 }, NaN { sign = s2, isSignaling = sn2, payload = a2 }) ->
                   compare s2 s1 <> reverseIf s1 (compare sn2 sn1 <> compare a1 a2)
                 (NaN { sign = s1 }, _) -> if s1 then LT else GT
                 (_, NaN { sign = s2 }) -> if s2 then GT else LT
                 _ -> error "unreachable"
    Just LT -> LT
    Just GT -> GT
    Just EQ -> case (x, y) of
                 (Infinity {}, Infinity {}) -> EQ
                 (Finite { sign = s1, exponent = e1, significand = n1 }, Finite { sign = s2, exponent = e2, significand = n2 }) -> reverseIf s1 $ compare e1 e2
                 _ -> error "unreachable"

type Decimal32 = Decimal 96 7
type Decimal64 = Decimal 384 16
type Decimal128 = Decimal 6144 34
{-
type DecimalInterchangeFormat k = Decimal (3 * 2 ^ (Div k 16 + 3)) (9 * Div k 32 - 2)
newtype Decimal_BinaryInteger k = BinaryFormat (Decimal (3 * 2 ^ (Div k 16 + 3)) (9 * Div k 32 - 2))
newtype Decimal_DenselyPackedDecimal k = DenselyPackedDecimal (Decimal (3 * 2 ^ (Div k 16 + 3)) (9 * Div k 32 - 2))
instance (Mod k 32 ~ 0, 32 <= k) => Storable (Decimal_BinaryInteger k)
instance (Mod k 32 ~ 0, 32 <= k) => Storable (Decimal_DenselyPackedDecimal k)
-}
