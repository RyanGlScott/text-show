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

Optional 'Show', 'Show1', and 'Show2' instances for functions.

/Since: 0.3/
-}
module Text.Show.Text.Functions (showbFunction) where

import Data.Text.Lazy.Builder (Builder)
import Prelude hiding (Show)
import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(..), Show2(..))

#include "inline.h"

-- | Convert a function to a 'Builder'.
-- 
-- /Since: 0.3/
showbFunction :: (a -> b) -> Builder
showbFunction = showb
{-# INLINE showbFunction #-}

instance Show (a -> b) where
    showbPrec = showbPrecWith undefined
    INLINE_INST_FUN(showb)

instance Show1 ((->) a) where
    showbPrecWith = showbPrecWith2 undefined
    INLINE_INST_FUN(showbPrecWith)

instance Show2 (->) where
    showbPrecWith2 _ _ _ _ = "<function>"
    INLINE_INST_FUN(showbPrecWith2)
