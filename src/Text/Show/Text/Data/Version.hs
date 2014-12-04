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
module Text.Show.Text.Data.Version (
      showbVersionPrec
    , showbVersionConcrete
    ) where

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Version (Version(..))

import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List ()
import Text.Show.Text.Utils ((<>), s)

-- | Convert a 'Version' to a 'Builder' with the given precedence.
showbVersionPrec :: Int -> Version -> Builder
showbVersionPrec p (Version b t) = showbParen (p > appPrec) $
        "Version {versionBranch = "
     <> showb b
     <> ", versionTags = "
     <> showb t
     <> s '}'
{-# INLINE showbVersionPrec #-}

-- | Provides one possible concrete representation for 'Version'.  For
-- a version with 'versionBranch' @= [1,2,3]@ and 'versionTags' 
-- @= [\"tag1\",\"tag2\"]@, the output will be @1.2.3-tag1-tag2@.
showbVersionConcrete :: Version -> Builder
showbVersionConcrete (Version branch tags)
    = mconcat (intersperse (s '.') $ map showb branch) <>
        mconcat (map ((s '-' <>) . fromString) tags)
{-# INLINE showbVersionConcrete #-}

instance Show Version where
    showbPrec = showbVersionPrec
    {-# INLINE showbPrec #-}