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

'TextShow' instance for 'Proxy'.

/Since: 2/
-}
module TextShow.Data.Proxy () where

import Data.Proxy (Proxy)

import TextShow.Classes (TextShow(..))
import TextShow.TH.Internal (deriveTextShow1, makeShowbPrec,
                             makeShowtPrec, makeShowtlPrec)

#include "inline.h"

-- | /Since: 2/
instance TextShow (Proxy s) where
    showbPrec  = $(makeShowbPrec  ''Proxy)
    showtPrec  = $(makeShowtPrec  ''Proxy)
    showtlPrec = $(makeShowtlPrec ''Proxy)
    INLINE_INST_FUN(showbPrec)
    INLINE_INST_FUN(showtPrec)
    INLINE_INST_FUN(showtlPrec)

-- | /Since: 2/
$(deriveTextShow1 ''Proxy)
