{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Maybe
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Maybe' values.

/Since: 0.3/
-}
module Text.Show.Text.Data.Maybe (showbMaybePrecWith) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (showbPrecWith)
import Text.Show.Text.TH.Internal (deriveShow, deriveShow1)

#include "inline.h"

-- | Convert a 'Maybe' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbMaybePrecWith :: (Int -> a -> Builder) -> Int -> Maybe a -> Builder
showbMaybePrecWith = showbPrecWith
{-# INLINE showbMaybePrecWith #-}

$(deriveShow  ''Maybe)
$(deriveShow1 ''Maybe)
