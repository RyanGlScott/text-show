{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Functions
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Optional 'TextShow', 'TextShow1', and 'TextShow2' instances for functions.

/Since: 2/
-}
module TextShow.Functions (showbFunction) where

import Data.Text.Lazy.Builder (Builder)
import TextShow.Classes (TextShow(showb, showbPrec), TextShow1(..), TextShow2(..))

#include "inline.h"

-- | Convert a function to a 'Builder'.
--
-- /Since: 2/
showbFunction :: (a -> b) -> Builder
showbFunction = showb
{-# INLINE showbFunction #-}

instance TextShow (a -> b) where
    showbPrec = showbPrecWith undefined
    INLINE_INST_FUN(showb)

instance TextShow1 ((->) a) where
    showbPrecWith = showbPrecWith2 undefined
    INLINE_INST_FUN(showbPrecWith)

instance TextShow2 (->) where
    showbPrecWith2 _ _ _ _ = "<function>"
    INLINE_INST_FUN(showbPrecWith2)
