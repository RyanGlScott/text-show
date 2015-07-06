{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,4,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Fingerprint
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Fingerprint' values.
This module only exports functions if using @base-4.4.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.Fingerprint (
#if !(MIN_VERSION_base(4,4,0))
    ) where
#else
      showbFingerprint
    ) where

import Data.Monoid.Compat ((<>))
import Data.Semigroup (timesN)
import Data.Text.Lazy.Builder (Builder, singleton)
import Data.Word (Word64)

import GHC.Fingerprint.Type (Fingerprint(..))

import TextShow.Classes (TextShow(showb))
import TextShow.Data.Integral (showbHex)
import TextShow.Utils (lengthB)

-- | Convert a 'Fingerprint' to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
showbFingerprint :: Fingerprint -> Builder
showbFingerprint (Fingerprint w1 w2) = hex16 w1 <> hex16 w2
  where
    hex16 :: Word64 -> Builder
    hex16 i = let hex = showbHex i
              in timesN (fromIntegral . max 0 $ 16 - lengthB hex) (singleton '0') <> hex

instance TextShow Fingerprint where
    showb = showbFingerprint
    {-# INLINE showb #-}
#endif
