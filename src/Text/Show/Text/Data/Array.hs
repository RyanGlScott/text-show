{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Array
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'Array' values.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Array (showbArrayPrec) where

import Data.Array (Array, assocs, bounds)
import Data.Ix (Ix)
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen, showbSpace)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Data.Tuple ()
import Text.Show.Text.Utils ((<>))

-- | Convert a 'Array' value to a 'Builder' with the given precedence.
showbArrayPrec :: (Show i, Show e, Ix i) => Int -> Array i e -> Builder
showbArrayPrec p a = showbParen (p > appPrec) $
       "array "
    <> showb (bounds a)
    <> showbSpace
    <> showb (assocs a)
{-# INLINE showbArrayPrec #-}

instance (Show i, Show e, Ix i) => Show (Array i e) where
    showbPrec = showbArrayPrec
    {-# INLINE showbPrec #-}