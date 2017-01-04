{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Ratio
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Ratio' values.

/Since: 2/
-}
module TextShow.Data.Ratio (showbRatioPrec) where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Real (Ratio(..), ratioPrec, ratioPrec1)

import TextShow.Classes (TextShow(..), showbParen)
#if MIN_VERSION_base(4,4,0)
import TextShow.Classes (TextShow1(..))
#endif
import TextShow.Data.Integral ()

#include "inline.h"

-- | Convert a 'Ratio' to a 'Builder' with the given precedence.
--
-- Note that on @base-4.3.0.0@, this function must have a @('Show' a, 'Integral' a)@
-- constraint instead of just a @('Show' a)@ constraint.
--
-- /Since: 2/
showbRatioPrec ::
#if MIN_VERSION_base(4,4,0)
                  TextShow a
#else
                  (TextShow a, Integral a)
#endif
               => Int -> Ratio a -> Builder
showbRatioPrec p (numer :% denom) = showbParen (p > ratioPrec) $
       showbPrec ratioPrec1 numer
    <> " % "
    <> showbPrec ratioPrec1 denom
{-# INLINE showbRatioPrec #-}

instance
#if MIN_VERSION_base(4,4,0)
         TextShow a
#else
         (TextShow a, Integral a)
#endif
      => TextShow (Ratio a) where
    {-# SPECIALIZE instance TextShow Rational #-}
    showbPrec = showbRatioPrec
    INLINE_INST_FUN(showbPrec)

#if MIN_VERSION_base(4,4,0)
instance TextShow1 Ratio where
    liftShowbPrec sp _ p (numer :% denom) = showbParen (p > ratioPrec) $
           sp ratioPrec1 numer
        <> " % "
        <> sp ratioPrec1 denom
    INLINE_INST_FUN(liftShowbPrec)
#endif
