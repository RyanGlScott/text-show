{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Dynamic
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Dynamic'.

/Since: 2/
-}
module TextShow.Data.Dynamic (showbDynamic) where

import Data.Dynamic (Dynamic, dynTypeRep)
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder)

import Prelude ()

import TextShow.Classes (TextShow(..))
import TextShow.Data.Typeable (showbTypeRepPrec)

#include "inline.h"

-- | Convert a 'Dynamic' value to a 'Builder'.
--
-- /Since: 2/
showbDynamic :: Dynamic -> Builder
showbDynamic dyn = "<<" <> showbTypeRepPrec 0 (dynTypeRep dyn) <> ">>"
{-# INLINE showbDynamic #-}

instance TextShow Dynamic where
    showb = showbDynamic
    INLINE_INST_FUN(showb)
