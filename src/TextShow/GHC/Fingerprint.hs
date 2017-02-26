{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,4,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Fingerprint
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Fingerprint'.
Only provided if using @base-4.4.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.Fingerprint () where

#if MIN_VERSION_base(4,4,0)
import Data.Monoid.Compat ((<>))
import Data.Semigroup (mtimesDefault)
import Data.Text.Lazy.Builder (Builder, singleton)
import Data.Word (Word64)

import GHC.Fingerprint.Type (Fingerprint(..))

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
#endif
