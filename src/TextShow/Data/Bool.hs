{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Bool
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Bool' values.

/Since: 2/
-}
module TextShow.Data.Bool (showbBool) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (showb)
import TextShow.TH.Internal (deriveTextShow)

-- | Convert a 'Bool' to a 'Builder'.
--
-- /Since: 2/
showbBool :: Bool -> Builder
showbBool = showb
{-# INLINE showbBool #-}

$(deriveTextShow ''Bool)
