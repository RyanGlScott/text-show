{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Enum
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'Enum' values.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Enum (showbBool, showbOrdering) where

import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb))

-- | Convert a 'Bool' to a 'Builder'.
showbBool :: Bool -> Builder
showbBool = build
{-# INLINE showbBool #-}

-- | Convert a 'Ordering' to a 'Builder'.
showbOrdering :: Ordering -> Builder
showbOrdering LT = "LT"
showbOrdering EQ = "EQ"
showbOrdering GT = "GT"
{-# INLINE showbOrdering #-}

instance Show Bool where
    showb = showbBool
    {-# INLINE showb #-}

instance Show Ordering where
    showb = showbOrdering
    {-# INLINE showb #-}