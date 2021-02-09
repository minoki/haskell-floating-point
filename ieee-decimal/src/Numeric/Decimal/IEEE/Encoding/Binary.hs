{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Decimal.IEEE.Encoding.Binary where
import           Control.Exception (assert)
import           Data.Bits
import           Data.Proxy
import           Data.Word
import           GHC.TypeNats
import           Numeric.Decimal.IEEE
import           Numeric.Natural
import           Prelude hiding (exponent, significand)

-- Binary encoding

decodeDecimal :: forall emax p w j i. (KnownNat emax, KnownNat p, KnownNat w, KnownNat j, Integral i, Bits i, p ~ (3 * j + 1)) => Proxy w -> Proxy j -> i -> Decimal emax p
decodeDecimal proxyW proxyJ bits
  = let -- g: combination field
        g = (bits `shiftR` trailingFieldWidth) .&. (bit combinationFieldWidth - 1) -- G_0..G_{w+4}
        sign = testBit bits (combinationFieldWidth + trailingFieldWidth)
    in if g `shiftR` w == 0x1f then
         -- NaN
         let isSignaling = testBit g (w - 1)
             payload = bits .&. (bit trailingFieldWidth - 1)
             -- payload: (10 * j) bits
             -- 2^(10 * j) = 1024^j > 10^(3 * j)
             payload' = if payload >= 10^(3 * j) then
                          -- non-canonical
                          0
                        else
                          payload
         in NaN { sign = sign, isSignaling = isSignaling, payload = fromIntegral payload' }
       else
         if g `shiftR` w == 0x1e then
           -- Infinity
           Infinity { sign = sign }
         else
           -- Finite
           let g01 = g `shiftR` (w + 3) -- G_0G_1
               (biasedExponent, significand) = if g01 <= 2 then
                                                 -- biasedExponent: G_0..G_{w+1}
                                                 -- significand: G_{w+2}..G_{w+4} <> T
                                                 (fromIntegral (g `shiftR` 3), bits .&. (bit (trailingFieldWidth + 3) - 1))
                                               else -- G_0G_1 == 0b11, G_2G_3 <= 2
                                                 -- biasedExponent: G2..G_{w+3}
                                                 -- significand: (8+G_{w+4}) <> T
                                                 (fromIntegral ((g `shiftR` 1) .&. (bit (w + 2) - 1)), bit (trailingFieldWidth + 3) .|. (bits .&. (bit (trailingFieldWidth + 1) - 1)))
               -- biasedExponent: (w+2)-bit, with first two bits either 0, 1, or 2
               -- significand: (10 * j + 4) bits
               -- 2^(10 * j + 4) = 16 * 1024^j > 10 * 10^(3 * j) = 10^(3 * j + 1)
               significand' = if significand >= 10^(3 * j + 1) then
                                -- non-canonical
                                0
                              else
                                significand
           in Finite { sign = sign, exponent = biasedExponent - bias, significand = fromIntegral significand' }
  where
    p = fromIntegral (natVal (Proxy :: Proxy p))
    emax = fromIntegral (natVal (Proxy :: Proxy emax))
    w = fromIntegral (natVal proxyW)
    j = fromIntegral (natVal proxyJ)
    combinationFieldWidth = w + 5
    trailingFieldWidth = 10 * j
    bias = emax + p - 2
{-# INLINE decodeDecimal #-}

encodeDecimal :: forall emax p w j i. (KnownNat emax, KnownNat p, KnownNat w, KnownNat j, Integral i, Bits i, p ~ (3 * j + 1)) => Proxy w -> Proxy j -> Decimal emax p -> i
encodeDecimal proxyW proxyJ value
  = let signBit = if sign value then
                    -- negative
                    bit (combinationFieldWidth + trailingFieldWidth)
                  else
                    -- positive
                    0
    in signBit .|. case value of
                     NaN { isSignaling = isSignaling, payload = payload } ->
                       let payload' = if payload < 10^(3 * j) then
                                        fromIntegral payload
                                      else
                                        10^(3 * j) - 1 -- should raise an error?
                       in if isSignaling then
                            (0x3f `shiftL` (combinationFieldWidth + trailingFieldWidth - 6)) .|. payload' -- G_0..G_5 = 0b111111
                          else
                            (0x3e `shiftL` (combinationFieldWidth + trailingFieldWidth - 6)) .|. payload' -- G_0..G_5 = 0b111111
                     Infinity {} -> (0x1e `shiftL` (combinationFieldWidth + trailingFieldWidth - 5)) -- G_0..G_4 = 0b11110
                     Finite { exponent = exponent, significand = significand } ->
                       let biasedExponent = exponent + bias
                           -- biasedExponent: (w+2)-bit, with first two bits either 0, 1, or 2
                           !_ = assert (biasedExponent >= 0 && biasedExponent < (3 `shiftL` w)) ()
                           -- significand < 10^p = 10^(3 * j + 1) < 10 * 1024^j
                       in if significand `shiftR` (trailingFieldWidth + 3) == 1 then
                            -- 8 * 1024^j = 2^(10 * j + 3) <= significand < 10 * 1024^j
                            let trailingField' = fromIntegral significand .&. (bit (trailingFieldWidth + 1) - 1) -- G_{w+4} <> T
                                -- G_0G_1 = 0b11, G_2..G_{w+3} = biasedExponent
                            in (3 `shiftL` (w + 3 + trailingFieldWidth)) .|. (fromIntegral biasedExponent `shiftL` (1 + trailingFieldWidth)) .|. trailingField'
                          else
                            -- significand < 8 * 1024^j = 2^(10 * j + 3)
                            (fromIntegral biasedExponent `shiftL` (trailingFieldWidth + 3)) .|. fromIntegral significand
  where
    p = fromIntegral (natVal (Proxy :: Proxy p)) -- 3 * J + 1
    emax = fromIntegral (natVal (Proxy :: Proxy emax))
    w = fromIntegral (natVal proxyW)
    j = fromIntegral (natVal proxyJ)
    combinationFieldWidth = w + 5
    trailingFieldWidth = 10 * j
    bias = emax + p - 2

decodeDecimal32 :: Word32 -> Decimal32
decodeDecimal32 = decodeDecimal proxyW proxyJ
  where
    -- k: 32 (storage width in bits)
    -- p: 7 (precision in digits)
    -- emax: 96
    -- bias: 101
    -- sign bit: 1
    -- w+5: 11 (combination field width in bits)
    -- t: 20 (trailing significand field width in bits)
    proxyW = Proxy :: Proxy 6 -- (w+5) - 5
    proxyJ = Proxy :: Proxy 2 -- t / 10

encodeDecimal32 :: Decimal32 -> Word32
encodeDecimal32 = encodeDecimal proxyW proxyJ
  where
    -- k: 32 (storage width in bits)
    -- p: 7 (precision in digits)
    -- emax: 96
    -- bias: 101
    -- sign bit: 1
    -- w+5: 11 (combination field width in bits)
    -- t: 20 (trailing significand field width in bits)
    proxyW = Proxy :: Proxy 6 -- (w+5) - 5
    proxyJ = Proxy :: Proxy 2 -- t / 10

decodeDecimal64 :: Word64 -> Decimal64
decodeDecimal64 = decodeDecimal proxyW proxyJ
  where
    -- k: 64 (storage width in bits)
    -- p: 16 (precision in digits)
    -- emax: 384
    -- bias: 398
    -- sign bit: 1
    -- w+5: 13 (combination field width in bits)
    -- t: 50 (trailing significand field width in bits)
    proxyW = Proxy :: Proxy 8 -- (w+5) - 5
    proxyJ = Proxy :: Proxy 5 -- t / 10

encodeDecimal64 :: Decimal64 -> Word64
encodeDecimal64 = encodeDecimal proxyW proxyJ
  where
    -- k: 64 (storage width in bits)
    -- p: 16 (precision in digits)
    -- emax: 384
    -- bias: 398
    -- sign bit: 1
    -- w+5: 13 (combination field width in bits)
    -- t: 50 (trailing significand field width in bits)
    proxyW = Proxy :: Proxy 8 -- (w+5) - 5
    proxyJ = Proxy :: Proxy 5 -- t / 10

decodeDecimal128 :: Natural -> Decimal128
decodeDecimal128 x | x < 2^128 = decodeDecimal proxyW proxyJ x
                   | otherwise = error "decodeDecimal128: out of range"
  where
    -- k: 128 (storage width in bits)
    -- p: 34 (precision in digits)
    -- emax: 6144
    -- bias: 6176
    -- sign bit: 1
    -- w+5: 17 (combination field width in bits)
    -- t: 110 (trailing significand field width in bits)
    proxyW = Proxy :: Proxy 12 -- (w+5) - 5
    proxyJ = Proxy :: Proxy 11 -- t / 10

encodeDecimal128 :: Decimal128 -> Natural
encodeDecimal128 = encodeDecimal proxyW proxyJ
  where
    -- k: 128 (storage width in bits)
    -- p: 34 (precision in digits)
    -- emax: 6144
    -- bias: 6176
    -- sign bit: 1
    -- w+5: 17 (combination field width in bits)
    -- t: 110 (trailing significand field width in bits)
    proxyW = Proxy :: Proxy 12 -- (w+5) - 5
    proxyJ = Proxy :: Proxy 11 -- t / 10
