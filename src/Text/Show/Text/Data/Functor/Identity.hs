{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Functor.Identity
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Identity' values.
-}
module Text.Show.Text.Data.Functor.Identity (showbIdentityPrec) where

import Data.Functor.Identity (Identity(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1), showbUnary)

#include "inline.h"

-- | Convert an 'Identity' value to a 'Builder' with the given precedence.
showbIdentityPrec :: Show a => Int -> Identity a -> Builder
-- This would be equivalent to the derived instance of 'Identity' if the
-- 'runIdentity' field were removed.
showbIdentityPrec p (Identity x) = showbUnary "Identity" p x
{-# INLINE showbIdentityPrec #-}

instance Show a => Show (Identity a) where
    showbPrec = showbIdentityPrec
    {-# INLINE showbPrec #-}

instance Show1 Identity where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)
