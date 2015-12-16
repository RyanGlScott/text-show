{-# LANGUAGE CPP       #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Numeric.Natural
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Natural's.

/Since: 2/
-}
module TextShow.Numeric.Natural (showbNaturalPrec) where

import Data.Text.Lazy.Builder (Builder)

#if MIN_VERSION_base(4,8,0)
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Natural (Natural(..))
import GHC.Types (Word(..))

import TextShow.Data.Integral (showbWord)
#else
import Numeric.Natural (Natural)
#endif

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral (showbIntegerPrec)

#include "inline.h"

-- | Convert a 'Natural' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbNaturalPrec :: Int -> Natural -> Builder
#if MIN_VERSION_base(4,8,0)
showbNaturalPrec _ (NatS# w#)  = showbWord $ W# w#
showbNaturalPrec p (NatJ# bn)  = showbIntegerPrec p $ Jp# bn
#else
showbNaturalPrec p             = showbIntegerPrec p . toInteger
{-# INLINE showbNaturalPrec #-}
#endif

instance TextShow Natural where
    showbPrec = showbNaturalPrec
    INLINE_INST_FUN(showbPrec)
