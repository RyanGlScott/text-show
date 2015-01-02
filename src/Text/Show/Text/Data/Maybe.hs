{-# LANGUAGE CPP, TemplateHaskell #-}
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
module Text.Show.Text.Data.Maybe (showbMaybePrec) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

#include "inline.h"

-- | Convert a 'Maybe' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbMaybePrec :: Show a => Int -> Maybe a -> Builder
showbMaybePrec = showbPrec
{-# INLINE showbMaybePrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Maybe)

instance Show1 Maybe where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)