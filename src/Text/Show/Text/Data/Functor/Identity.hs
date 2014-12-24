{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverloadedStrings #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
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

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))

#if MIN_VERSION_base(4,8,0)
import GHC.Show (appPrec, appPrec1)

import Text.Show.Text.Classes (showbParen)
import Text.Show.Text.Utils ((<>))
#else
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)
#endif

#include "inline.h"

-- Convert an 'Identity' value to a 'Builder' with the given precedence.
showbIdentityPrec :: Show a => Int -> Identity a -> Builder
#if MIN_VERSION_base(4,8,0)
-- This would be equivalent to the derived instance of 'Identity' if the
-- 'runIdentity' field were removed.
showbIdentityPrec p (Identity x) = showbParen (p > appPrec) $
    "Identity " <> showbPrec appPrec1 x
#else
showbIdentityPrec = showbPrec
#endif
{-# INLINE showbIdentityPrec #-}

#if MIN_VERSION_base(4,8,0)
instance Show a => Show (Identity a) where
    showbPrec = showbIdentityPrec
    {-# INLINE showbPrec #-}
#else
$(deriveShowPragmas defaultInlineShowbPrec ''Identity)
#endif

instance Show1 Identity where
    showbPrec1 = showbPrec
    INLINE(showbPrec1)
