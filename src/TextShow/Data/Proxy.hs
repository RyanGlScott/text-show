{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds         #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Proxy
Copyright:   (C) 2014-2017 Ryan Scott
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

import TextShow.Classes (TextShow(..))
import TextShow.TH.Internal (deriveTextShow1, makeShowbPrec,
                             makeShowtPrec, makeShowtlPrec)

#include "inline.h"

-- | Convert a 'Proxy' type to a 'Builder'.
--
-- /Since: 2/
showbProxy :: Proxy s -> Builder
showbProxy = showb
{-# INLINE showbProxy #-}

instance TextShow (Proxy s) where
    showbPrec  = $(makeShowbPrec  ''Proxy)
    showtPrec  = $(makeShowtPrec  ''Proxy)
    showtlPrec = $(makeShowtlPrec ''Proxy)
    INLINE_INST_FUN(showbPrec)
    INLINE_INST_FUN(showtPrec)
    INLINE_INST_FUN(showtlPrec)

$(deriveTextShow1 ''Proxy)
