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

'TextShow' instance for 'Dynamic'.

/Since: 2/
-}
module TextShow.Data.Dynamic () where

import Data.Dynamic (Dynamic, dynTypeRep)
import Data.Monoid.Compat ((<>))

import Prelude ()

import TextShow.Classes (TextShow(..))
import TextShow.Data.Typeable ()

#include "inline.h"

-- | /Since: 2/
instance TextShow Dynamic where
    showb dyn = "<<" <> showb (dynTypeRep dyn) <> ">>"
    INLINE_INST_FUN(showb)
