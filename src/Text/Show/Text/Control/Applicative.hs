{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
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

import Text.Show.Text.Class (Show(showbPrec))
import Text.Show.Text.Data.List ()
import Text.Show.Text.TH.Internal (deriveShow)

-- | Convert a 'ZipList' to a 'Builder' with the given precedence.
showbZipListPrec :: Show a => Int -> ZipList a -> Builder
showbZipListPrec = showbPrec
{-# INLINE showbZipListPrec #-}

$(deriveShow ''ZipList)