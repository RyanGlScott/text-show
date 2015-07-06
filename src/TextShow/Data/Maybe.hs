{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Maybe
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Maybe' values.

/Since: 2/
-}
module TextShow.Data.Maybe (showbMaybePrecWith) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (showbPrecWith)
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

#include "inline.h"

-- | Convert a 'Maybe' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbMaybePrecWith :: (Int -> a -> Builder) -> Int -> Maybe a -> Builder
showbMaybePrecWith = showbPrecWith
{-# INLINE showbMaybePrecWith #-}

$(deriveTextShow  ''Maybe)
$(deriveTextShow1 ''Maybe)
