{-# LANGUAGE CPP #-}

-- castFloatToWord32 is buggy on GHC <= 8.8 on x86_64.
-- See https://gitlab.haskell.org/ghc/ghc/issues/16617

#if MIN_VERSION_base(4,14,0) || !defined(x86_64_HOST_ARCH)

module GHC.Float.Compat (module GHC.Float) where
import GHC.Float

#else

module GHC.Float.Compat (module GHC.Float, castFloatToWord32) where
import           GHC.Float hiding (castFloatToWord32)
import qualified GHC.Float as F
import           Data.Bits ((.&.))
import           Data.Word (Word32)

-- Let's hope the compiler is not smart enough to eliminate the bit-and...
-- Or @fromIntegral (fromIntegral x :: Int) :: Word32@ might be better?
castFloatToWord32 :: Float -> Word32
castFloatToWord32 x = F.castFloatToWord32 x .&. 0xFFFFFFFF

#endif
