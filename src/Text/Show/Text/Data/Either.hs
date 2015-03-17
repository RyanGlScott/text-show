{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Either
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Either' values.

/Since: 0.3/
-}
module Text.Show.Text.Data.Either (showbEitherPrec) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

#include "inline.h"

-- | Convert a 'Either' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbEitherPrec :: (Show a, Show b) => Int -> Either a b -> Builder
showbEitherPrec = showbPrec
{-# INLINE showbEitherPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Either)

instance Show a => Show1 (Either a) where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)
