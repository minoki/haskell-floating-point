{-# LANGUAGE CPP #-}

-- castFloatToWord32 is buggy on GHC <= 8.8 && 64-bit systems.
-- See https://gitlab.haskell.org/ghc/ghc/issues/16617

#include "MachDeps.h"

#if MIN_VERSION_base(4,14,0) || WORD_SIZE_IN_BITS == 32

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
