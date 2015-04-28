{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Dynamic
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Dynamic'.

/Since: 0.3/
-}
module Text.Show.Text.Data.Dynamic (showbDynamic) where

import Data.Dynamic (Dynamic, dynTypeRep)
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder)

import Prelude ()

import Text.Show.Text.Classes (Show(showb))
import Text.Show.Text.Data.Typeable (showbTypeRepPrec)

#include "inline.h"

-- | Convert a 'Dynamic' value to a 'Builder'.
-- 
-- /Since: 0.3/
showbDynamic :: Dynamic -> Builder
showbDynamic dyn = "<<" <> showbTypeRepPrec 0 (dynTypeRep dyn) <> ">>"
{-# INLINE showbDynamic #-}

instance Show Dynamic where
    showb = showbDynamic
    INLINE_INST_FUN(showb)
