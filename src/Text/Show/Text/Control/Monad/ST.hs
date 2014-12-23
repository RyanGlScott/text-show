{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Monad.ST
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for strict 'ST' values.
-}
module Text.Show.Text.Control.Monad.ST (showbST) where

import Control.Monad.ST (ST)
import Data.Text.Lazy.Builder (Builder)
import Prelude hiding (Show)
import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))

#include "inline.h"

-- | Convert a strict 'ST' value to a 'Builder'.
showbST :: ST s a -> Builder
showbST _ = "<<ST action>>"
{-# INLINE showbST #-}

instance Show (ST s a) where
    showb = showbST
    INLINE(showb)

instance Show1 (ST s) where
    showbPrec1 = showbPrec
    INLINE(showbPrec1)