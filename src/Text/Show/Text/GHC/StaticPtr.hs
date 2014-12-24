{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.StaticPtr
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'StaticPtrInfo' values.
-}
module Text.Show.Text.GHC.StaticPtr (showbStaticPtrInfoPrec) where

import Data.Text.Lazy.Builder (Builder)

import GHC.StaticPtr (StaticPtrInfo)

import Text.Show.Text.Classes (showbPrec)
import Text.Show.Text.Data.Char     ()
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List     ()
import Text.Show.Text.Data.Tuple    ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

-- | Conver a 'StaticPtrInfo' value to a 'Builder' with the given precedence.
showbStaticPtrInfoPrec :: Int -> StaticPtrInfo -> Builder
showbStaticPtrInfoPrec = showbPrec
{-# INLINE showbStaticPtrInfoPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''StaticPtrInfo)
