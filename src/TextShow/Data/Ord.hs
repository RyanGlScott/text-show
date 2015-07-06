{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Ord
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Ordering' and 'Down'.

/Since: 2/
-}
module TextShow.Data.Ord (
      showbOrdering
#if MIN_VERSION_base(4,6,0)
    , showbDownPrecWith
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (showb)
import TextShow.TH.Internal (deriveTextShow)

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)

import TextShow.Classes (showbPrecWith)
import TextShow.TH.Internal (deriveTextShow1)
#endif

#include "inline.h"

-- | Convert a 'Ordering' to a 'Builder'.
--
-- /Since: 2/
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

#if MIN_VERSION_base(4,6,0)
-- | Convert a 'Down' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.6.0.0@ or later.
--
-- /Since: 2/
showbDownPrecWith :: (Int -> a -> Builder) -> Int -> Down a -> Builder
showbDownPrecWith = showbPrecWith
{-# INLINE showbDownPrecWith #-}
#endif

$(deriveTextShow  ''Ordering)

#if MIN_VERSION_base(4,6,0)
$(deriveTextShow  ''Down)
$(deriveTextShow1 ''Down)
#endif
