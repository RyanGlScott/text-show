{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Proxy
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Proxy' values.

/Since: 2/
-}
module TextShow.Data.Proxy (showbProxy) where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(showb, showbPrec))
import TextShow.TH.Internal (deriveTextShow1, makeShowbPrec)

#include "inline.h"

-- | Convert a 'Proxy' type to a 'Builder'.
--
-- /Since: 2/
showbProxy :: Proxy s -> Builder
showbProxy = showb
{-# INLINE showbProxy #-}

instance TextShow (Proxy s) where
    showbPrec = $(makeShowbPrec ''Proxy)
    INLINE_INST_FUN(showbPrec)

$(deriveTextShow1 ''Proxy)
