{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Dynamic
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Dynamic'.
-}
module Text.Show.Text.Data.Dynamic (showbDynamic) where

import Data.Dynamic (Dynamic, dynTypeRep)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb))
import Text.Show.Text.Data.Typeable (showbTypeRepPrec)
import Text.Show.Text.Utils ((<>))

-- | Convert a 'Dynamic' value to a 'Builder'.
showbDynamic :: Dynamic -> Builder
showbDynamic dyn = "<<" <> showbTypeRepPrec 0 (dynTypeRep dyn) <> ">>"
{-# INLINE showbDynamic #-}

instance Show Dynamic where
    showb = showbDynamic
    {-# INLINE showb #-}