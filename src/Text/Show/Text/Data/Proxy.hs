{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Proxy
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Proxy' values.
-}
module Text.Show.Text.Data.Proxy (showbProxy) where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (mkShowbPrec)

-- | Convert a 'Proxy' type to a 'Builder'.
showbProxy :: Proxy s -> Builder
showbProxy = showb
{-# INLINE showbProxy #-}

-- TODO: See why 'deriveShow' can't detect Proxy's phantom type correctly
instance Show (Proxy s) where
    showbPrec = $(mkShowbPrec ''Proxy)
    {-# INLINE showb #-}

instance Show1 Proxy where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}