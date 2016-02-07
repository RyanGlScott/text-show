{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Functions
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Optional 'TextShow', 'TextShow1', and 'TextShow2' instances for functions.

/Since: 2/
-}
module TextShow.Functions (showbFunction) where

import Data.Text.Lazy.Builder (Builder)
import TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..))

#include "inline.h"

-- | Convert a function to a 'Builder'.
--
-- /Since: 2/
showbFunction :: (a -> b) -> Builder
showbFunction = showb
{-# INLINE showbFunction #-}

instance TextShow (a -> b) where
    showbPrec = liftShowbPrec undefined undefined
    INLINE_INST_FUN(showbPrec)

instance TextShow1 ((->) a) where
    liftShowbPrec = liftShowbPrec2 undefined undefined
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 (->) where
    liftShowbPrec2 _ _ _ _ _ _ = "<function>"
    INLINE_INST_FUN(liftShowbPrec2)
