{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Proxy
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Proxy' values.

/Since: 0.4/
-}
module Text.Show.Text.Data.Proxy (showbProxy) where

import Data.Proxy (Proxy(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (mkShowbPrec)

#include "inline.h"

-- | Convert a 'Proxy' type to a 'Builder'.
-- 
-- /Since: 0.4/
showbProxy :: Proxy s -> Builder
showbProxy = showb
{-# INLINE showbProxy #-}

-- TODO: Derive with TH once it can detect phantom types properly
instance Show (Proxy s) where
    showbPrec = $(mkShowbPrec ''Proxy)
    INLINE_INST_FUN(showb)

instance Show1 Proxy where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)