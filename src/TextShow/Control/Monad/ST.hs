{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Monad.ST
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for strict 'ST' values.

/Since: 2/
-}
module TextShow.Control.Monad.ST (showbST) where

import Control.Monad.ST (ST)
import Data.Text.Lazy.Builder (Builder)
import TextShow.Classes (TextShow(showb), TextShow1(..), TextShow2(..))

#include "inline.h"

-- | Convert a strict 'ST' value to a 'Builder'.
--
-- /Since: 2/
showbST :: ST s a -> Builder
showbST = showb
{-# INLINE showbST #-}

instance TextShow (ST s a) where
    showb = showbPrecWith undefined 0
    INLINE_INST_FUN(showb)

instance TextShow1 (ST s) where
    showbPrecWith = showbPrecWith2 undefined
    INLINE_INST_FUN(showbPrecWith)

instance TextShow2 ST where
    showbPrecWith2 _ _ _ _ = "<<ST action>>"
    INLINE_INST_FUN(showbPrecWith2)
