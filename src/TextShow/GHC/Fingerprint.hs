{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.GHC.Fingerprint
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Fingerprint'.

/Since: 2/
-}
module TextShow.GHC.Fingerprint () where

import Data.Semigroup.Compat (mtimesDefault)
import Data.Text.Lazy.Builder (Builder, singleton)
import Data.Word (Word64)

import GHC.Fingerprint.Type (Fingerprint(..))

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral (showbHex)
import TextShow.Utils (lengthB)

-- | /Since: 2/
instance TextShow Fingerprint where
    showb (Fingerprint w1 w2) = hex16 w1 <> hex16 w2
      where
        hex16 :: Word64 -> Builder
        hex16 i = let hex = showbHex i
                  in mtimesDefault (max 0 $ 16 - lengthB hex) (singleton '0') <> hex
