{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Proxy
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'Proxy' values.

/Since: 0.4/
-}
module Text.Show.Text.Data.Proxy (showbProxy) where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), showbPrecWith)
import Text.Show.Text.TH.Internal (deriveShow1)

#include "inline.h"

-- | Convert a 'Proxy' type to a 'Builder'.
--
-- /Since: 0.4/
showbProxy :: Proxy s -> Builder
showbProxy = showb
{-# INLINE showbProxy #-}

instance Show (Proxy s) where
    showbPrec = showbPrecWith undefined
    INLINE_INST_FUN(showb)

$(deriveShow1 ''Proxy)
