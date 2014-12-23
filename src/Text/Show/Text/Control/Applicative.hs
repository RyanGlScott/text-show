{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Applicative
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ZipList'.
-}
module Text.Show.Text.Control.Applicative (showbZipListPrec) where

import Control.Applicative (ZipList)

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.Data.List ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

#include "inline.h"

-- | Convert a 'ZipList' to a 'Builder' with the given precedence.
showbZipListPrec :: Show a => Int -> ZipList a -> Builder
showbZipListPrec = showbPrec
{-# INLINE showbZipListPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''ZipList)

instance Show1 ZipList where
    showbPrec1 = showbPrec
    INLINE(showbPrec1)