{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ratio
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Ratio' values.

/Since: 0.5/
-}
module Text.Show.Text.Data.Ratio (showbRatioPrec) where

import Data.Text.Lazy.Builder (Builder)
import Data.Ratio (Ratio, numerator, denominator)

import GHC.Real (ratioPrec, ratioPrec1)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), showbParen)
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Utils ((<>))

#include "inline.h"

-- | Convert a 'Ratio' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.5/
showbRatioPrec :: (Show a, Integral a) => Int -> Ratio a -> Builder
showbRatioPrec p q = showbParen (p > ratioPrec) $
       showbPrec ratioPrec1 (numerator q)
    <> " % "
    <> showbPrec ratioPrec1 (denominator q)
{-# INLINE showbRatioPrec #-}

instance (Show a, Integral a) => Show (Ratio a) where
    {-# SPECIALIZE instance Show Rational #-}
    showbPrec = showbRatioPrec
    INLINE_INST_FUN(showbPrec)
