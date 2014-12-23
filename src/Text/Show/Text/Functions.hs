{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Functions
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Optional 'Show' and 'Show1' instances for functions.
-}
module Text.Show.Text.Functions (showbFunction) where

import Data.Text.Lazy.Builder (Builder)
import Prelude hiding (Show)
import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))

#include "inline.h"

-- | Convert a function to a 'Builder'.
showbFunction :: (a -> b) -> Builder
showbFunction _ = "<function>"
{-# INLINE showbFunction #-}

instance Show (a -> b) where
    showb = showbFunction
    INLINE(showb)

instance Show1 ((->) a) where
    showbPrec1 = showbPrec
    INLINE(showbPrec1)