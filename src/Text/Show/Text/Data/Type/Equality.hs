{-# LANGUAGE GADTs, NoImplicitPrelude, OverloadedStrings, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Type.Equality
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for propositional equality.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Type.Equality (showbPropEquality) where

import Data.Text.Lazy.Builder (Builder)
import Data.Type.Equality ((:~:)(..))

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb))

-- | Convert a propositional equality value to a 'Builder'.
showbPropEquality :: (a :~: b) -> Builder
showbPropEquality Refl = "Refl"
{-# INLINE showbPropEquality #-}

instance Show (a :~: b) where
    showb = showbPropEquality
    {-# INLINE showb #-}