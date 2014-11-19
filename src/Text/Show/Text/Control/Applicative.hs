{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Control.Applicative
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'ZipList'.
----------------------------------------------------------------------------
module Text.Show.Text.Control.Applicative (showbZipListPrec) where

import Control.Applicative (ZipList(..))

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Utils (s)

-- | Convert a 'ZipList' to a 'Builder' with the given precedence.
showbZipListPrec :: Show a => Int -> ZipList a -> Builder
showbZipListPrec p (ZipList zl) = showbParen (p > appPrec) $
        "ZipList {getZipList = "
     <> showb zl
     <> s '}'
{-# INLINE showbZipListPrec #-}

instance Show a => Show (ZipList a) where
    showbPrec = showbZipListPrec
    {-# INLINE showbPrec #-}