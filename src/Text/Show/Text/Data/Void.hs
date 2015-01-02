{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Void
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Void' values.

/Since: 0.5/
-}
module Text.Show.Text.Data.Void (showbVoid) where

import Data.Text.Lazy.Builder (Builder)
import Data.Void (Void, absurd)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb))

#include "inline.h"

-- | Since 'Void' values logically don't exist, attempting to convert one to a
-- 'Builder' will never terminate.
-- 
-- /Since: 0.5/
showbVoid :: Void -> Builder
showbVoid = absurd
{-# INLINE showbVoid #-}

instance Show Void where
    showb = showbVoid
    INLINE_INST_FUN(showb)
