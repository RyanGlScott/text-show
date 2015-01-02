{-# LANGUAGE CPP #-}

#if defined(MIN_VERSION_integer_gmp)
# define HAVE_GMP_BIGNAT MIN_VERSION_integer_gmp(1,0,0)
#else
# define HAVE_GMP_BIGNAT 0
#endif

#if HAVE_GMP_BIGNAT
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Numeric.Natural
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Natural's.

/Since: 0.5/
-}
module Text.Show.Text.Numeric.Natural (showbNaturalPrec) where

import Data.Text.Lazy.Builder (Builder)

#if HAVE_GMP_BIGNAT
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Types (Word(..))

import Text.Show.Text.Data.Integral (showbWord)
#endif

#if MIN_VERSION_base(4,8,0)
import GHC.Natural (Natural(..))
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
# if HAVE_GMP_BIGNAT
showbNaturalPrec _ (NatS# w#)  = showbWord $ W# w#
showbNaturalPrec p (NatJ# bn)  = showbIntegerPrec p $ Jp# bn
# else
showbNaturalPrec p (Natural i) = showbIntegerPrec p i
{-# INLINE showbNaturalPrec #-}
# endif
#else
showbNaturalPrec p = showbIntegerPrec p . toInteger
{-# INLINE showbNaturalPrec #-}
#endif

instance Show Natural where
    showbPrec = showbNaturalPrec
    INLINE_INST_FUN(showbPrec)
