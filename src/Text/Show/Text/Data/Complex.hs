{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ratio
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Ratio' values.
-}
module Text.Show.Text.Data.Complex (showbComplexPrec) where

import Data.Complex (Complex)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec))
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.TH.Internal (mkShowbPrec)

-- | Convert a 'Complex' value to a 'Builder' with the given precedence.
showbComplexPrec :: (RealFloat a, Show a) => Int -> Complex a -> Builder
showbComplexPrec = showbPrec
{-# INLINE showbComplexPrec #-}

instance (RealFloat a, Show a) => Show (Complex a) where
    {-# SPECIALIZE instance Show (Complex Float) #-}
    {-# SPECIALIZE instance Show (Complex Double) #-}
    showbPrec = $(mkShowbPrec ''Complex)
    {-# INLINE showbPrec #-}