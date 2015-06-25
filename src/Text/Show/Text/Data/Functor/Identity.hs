{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Functor.Identity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'Identity' values.

/Since: 0.5/
-}
module Text.Show.Text.Data.Functor.Identity (showbIdentityPrecWith) where

import Data.Functor.Identity (Identity(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(..), showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert an 'Identity' value to a 'Builder' with the given show function
-- and precedence.
-- 
-- /Since: 1/
showbIdentityPrecWith :: (Int -> a -> Builder) -> Int -> Identity a -> Builder
-- This would be equivalent to the derived instance of 'Identity' if the
-- 'runIdentity' field were removed.
showbIdentityPrecWith sp p (Identity x) = showbUnaryWith sp "Identity" p x
{-# INLINE showbIdentityPrecWith #-}

instance Show a => Show (Identity a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

instance Show1 Identity where
    showbPrecWith = showbIdentityPrecWith
    INLINE_INST_FUN(showbPrecWith)
