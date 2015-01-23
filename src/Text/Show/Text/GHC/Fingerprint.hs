{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.Fingerprint
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Fingerprint' values.
This module is only available with @base-4.4.0.0@ or later.

/Since: 0.5/
-}
module Text.Show.Text.GHC.Fingerprint (showbFingerprint) where

import Data.Semigroup (timesN)
import Data.Text.Lazy.Builder (Builder)
import Data.Word (Word64)

import GHC.Fingerprint.Type (Fingerprint(..))

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb))
import Text.Show.Text.Data.Integral (showbHex)
import Text.Show.Text.Utils ((<>), lengthB, s)

-- | Convert a 'Fingerprint' to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.5/
showbFingerprint :: Fingerprint -> Builder
showbFingerprint (Fingerprint w1 w2) = hex16 w1 <> hex16 w2
  where
    hex16 :: Word64 -> Builder
    hex16 i = let hex = showbHex i
              in timesN (fromIntegral . max 0 $ 16 - lengthB hex) (s '0') <> hex

instance Show Fingerprint where
    showb = showbFingerprint
    {-# INLINE showb #-}
