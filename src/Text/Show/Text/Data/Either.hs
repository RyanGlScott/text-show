{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Either
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Either' values.
-}
module Text.Show.Text.Data.Either (showbEitherPrec) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec))
import Text.Show.Text.TH.Internal (deriveShow)

-- | Convert a 'Either' value to a 'Builder' with the given precedence.
showbEitherPrec :: (Show a, Show b) => Int -> Either a b -> Builder
showbEitherPrec = showbPrec
{-# INLINE showbEitherPrec #-}

$(deriveShow ''Either)