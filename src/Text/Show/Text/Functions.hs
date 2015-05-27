{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Functions
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Optional 'Show' and 'Show1' instances for functions.

/Since: 0.3/
-}
module Text.Show.Text.Functions (showbFunction) where

import Data.Text.Lazy.Builder (Builder)
import Prelude ()
import Text.Show.Text.Classes (Show(showb))

#include "inline.h"

-- | Convert a function to a 'Builder'.
-- 
-- /Since: 0.3/
showbFunction :: (a -> b) -> Builder
showbFunction _ = "<function>"
{-# INLINE showbFunction #-}

instance Show (a -> b) where
    showb = showbFunction
    INLINE_INST_FUN(showb)

-- instance Show1 ((->) a) where
--     showbPrec1 = showbPrec
--     INLINE_INST_FUN(showbPrec1)
