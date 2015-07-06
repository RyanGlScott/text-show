{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Functor.Identity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Identity' values.

/Since: 2/
-}
module TextShow.Data.Functor.Identity (showbIdentityPrecWith) where

import Data.Functor.Identity (Identity(..))
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(showbPrec), TextShow1(..),
                         showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert an 'Identity' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbIdentityPrecWith :: (Int -> a -> Builder) -> Int -> Identity a -> Builder
-- This would be equivalent to the derived instance of 'Identity' if the
-- 'runIdentity' field were removed.
showbIdentityPrecWith sp p (Identity x) = showbUnaryWith sp "Identity" p x
{-# INLINE showbIdentityPrecWith #-}

instance TextShow a => TextShow (Identity a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

instance TextShow1 Identity where
    showbPrecWith = showbIdentityPrecWith
    INLINE_INST_FUN(showbPrecWith)
