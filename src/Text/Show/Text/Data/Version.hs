{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Version
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'Version'.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Version (showbVersionPrec) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import Data.Version (Version(..))

import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List ()
import Text.Show.Text.Utils (s)

-- | Convert a 'Version' to a 'Builder' with the given precedence.
showbVersionPrec :: Int -> Version -> Builder
showbVersionPrec p (Version b t) = showbParen (p > appPrec) $
        "Version {versionBranch = "
     <> showb b
     <> ", versionTags = "
     <> showb t
     <> s '}'
{-# INLINE showbVersionPrec #-}

instance Show Version where
    showbPrec = showbVersionPrec
    {-# INLINE showbPrec #-}