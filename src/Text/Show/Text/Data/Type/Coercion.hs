{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Type.Coercion
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for representational equality.
This module is only available with @base-4.7.0.0@ or later.

/Since: 0.3/
-}
module Text.Show.Text.Data.Type.Coercion (showbCoercion) where

import Data.Text.Lazy.Builder (Builder)
import Data.Type.Coercion (Coercion(..))

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (mkShowbPrec)

-- | Convert a representational equality value to a 'Builder'.
-- This function is only available with @base-4.7.0.0@ or later.
-- 
-- /Since: 0.3/
showbCoercion :: Coercion a b -> Builder
showbCoercion = showb
{-# INLINE showbCoercion #-}

-- TODO: See why 'deriveShow' doesn't detect that b is a phantom type
instance Show (Coercion a b) where
    showbPrec = $(mkShowbPrec ''Coercion)
    {-# INLINE showb #-}

instance Show1 (Coercion a) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}
