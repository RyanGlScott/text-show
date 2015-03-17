{-# LANGUAGE CPP       #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Numeric.Natural
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Natural's.

/Since: 0.5/
-}
module Text.Show.Text.Numeric.Natural (showbNaturalPrec) where

import Data.Text.Lazy.Builder (Builder)

#if MIN_VERSION_base(4,8,0)
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Natural (Natural(..))
import GHC.Types (Word(..))

import Text.Show.Text.Data.Integral (showbWord)
#else
import Numeric.Natural (Natural)
#endif

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec))
import Text.Show.Text.Data.Integral (showbIntegerPrec)

#include "inline.h"

-- | Convert a 'Natural' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.5/
showbNaturalPrec :: Int -> Natural -> Builder
#if MIN_VERSION_base(4,8,0)
showbNaturalPrec _ (NatS# w#)  = showbWord $ W# w#
showbNaturalPrec p (NatJ# bn)  = showbIntegerPrec p $ Jp# bn
#else
showbNaturalPrec p             = showbIntegerPrec p . toInteger
{-# INLINE showbNaturalPrec #-}
#endif

instance Show Natural where
    showbPrec = showbNaturalPrec
    INLINE_INST_FUN(showbPrec)
