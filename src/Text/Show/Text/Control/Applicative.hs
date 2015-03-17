{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Applicative
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ZipList'.

/Since: 0.3/
-}
module Text.Show.Text.Control.Applicative (showbConstPrec, showbZipListPrec) where

import Control.Applicative (Const(..), ZipList)

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1), showbUnary)
import Text.Show.Text.Data.List ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

#include "inline.h"

showbConstPrec :: Show a => Int -> Const a b -> Builder
showbConstPrec p (Const x) = showbUnary "Const" p x
{-# INLINE showbConstPrec #-}

-- | Convert a 'ZipList' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbZipListPrec :: Show a => Int -> ZipList a -> Builder
showbZipListPrec = showbPrec
{-# INLINE showbZipListPrec #-}

instance Show a => Show (Const a b) where
    showbPrec = showbConstPrec
    INLINE_INST_FUN(showbPrec)

instance Show a => Show1 (Const a) where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

$(deriveShowPragmas defaultInlineShowbPrec ''ZipList)

instance Show1 ZipList where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)
