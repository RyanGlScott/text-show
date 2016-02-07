{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Monad.ST
Copyright:   (C) 2014-2016 Ryan Scott
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
import TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..))

#include "inline.h"

-- | Convert a strict 'ST' value to a 'Builder'.
--
-- /Since: 2/
showbST :: ST s a -> Builder
showbST = showb
{-# INLINE showbST #-}

instance TextShow (ST s a) where
    showb = liftShowbPrec undefined undefined 0
    INLINE_INST_FUN(showb)

instance TextShow1 (ST s) where
    liftShowbPrec = liftShowbPrec2 undefined undefined
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 ST where
    liftShowbPrec2 _ _ _ _ _ _ = "<<ST action>>"
    INLINE_INST_FUN(liftShowbPrec2)
