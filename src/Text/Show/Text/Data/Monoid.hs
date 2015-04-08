{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Monoid
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Monoid'-related newtypes.

/Since: 0.3/
-}
module Text.Show.Text.Data.Monoid (
      showbAllPrec
    , showbAnyPrec
    , showbDualPrec
    , showbFirstPrec
    , showbLastPrec
    , showbProductPrec
    , showbSumPrec
    , showbAltPrec
    ) where

import Data.Monoid.Compat (All, Any, Dual, First, Last, Product, Sum, Alt)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.Data.Bool ()
import Text.Show.Text.Data.Maybe ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec,
                                   mkShowbPrec)

#include "inline.h"

-- | Convert an 'All' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbAllPrec :: Int -> All -> Builder
showbAllPrec = showbPrec
{-# INLINE showbAllPrec #-}

-- | Convert an 'Any' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbAnyPrec :: Int -> Any -> Builder
showbAnyPrec = showbPrec
{-# INLINE showbAnyPrec #-}

-- | Convert a 'Dual' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbDualPrec :: Show a => Int -> Dual a -> Builder
showbDualPrec = showbPrec
{-# INLINE showbDualPrec #-}

-- | Convert a 'First' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbFirstPrec :: Show a => Int -> First a -> Builder
showbFirstPrec = showbPrec
{-# INLINE showbFirstPrec #-}

-- | Convert a 'Last' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbLastPrec :: Show a => Int -> Last a -> Builder
showbLastPrec = showbPrec
{-# INLINE showbLastPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbProductPrec :: Show a => Int -> Product a -> Builder
showbProductPrec = showbPrec
{-# INLINE showbProductPrec #-}

-- | Convert a 'Sum' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbSumPrec :: Show a => Int -> Sum a -> Builder
showbSumPrec = showbPrec
{-# INLINE showbSumPrec #-}

-- | Convert an 'Alt' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.8/
showbAltPrec :: Show (f a) => Int -> Alt f a -> Builder
showbAltPrec = showbPrec
{-# INLINE showbAltPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''All)
$(deriveShowPragmas defaultInlineShowbPrec ''Any)
$(deriveShowPragmas defaultInlineShowbPrec ''Dual)
$(deriveShowPragmas defaultInlineShowbPrec ''First)
$(deriveShowPragmas defaultInlineShowbPrec ''Last)
$(deriveShowPragmas defaultInlineShowbPrec ''Product)
$(deriveShowPragmas defaultInlineShowbPrec ''Sum)

-- TODO: Derive with TH once it can detect higher-kinded types properly
instance Show (f a) => Show (Alt f a) where
    showbPrec = $(mkShowbPrec ''Alt)
    {-# INLINE showbPrec #-}

instance Show1 Dual where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 First where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Last where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Product where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Sum where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)
