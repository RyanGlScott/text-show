{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Monoid
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Monoid'-related newtypes.
-}
module Text.Show.Text.Data.Monoid (
      showbAllPrec
    , showbAnyPrec
    , showbDualPrec
    , showbFirstPrec
    , showbLastPrec
    , showbProductPrec
    , showbSumPrec
    ) where

import Data.Monoid (All, Any, Dual, First, Last, Product, Sum)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.Data.Bool ()
import Text.Show.Text.Data.Maybe ()
import Text.Show.Text.TH.Internal

-- | Convert an 'All' value to a 'Builder' with the given precedence.
showbAllPrec :: Int -> All -> Builder
showbAllPrec = showbPrec
{-# INLINE showbAllPrec #-}

-- | Convert an 'Any' value to a 'Builder' with the given precedence.
showbAnyPrec :: Int -> Any -> Builder
showbAnyPrec = showbPrec
{-# INLINE showbAnyPrec #-}

-- | Convert a 'Dual' value to a 'Builder' with the given precedence.
showbDualPrec :: Show a => Int -> Dual a -> Builder
showbDualPrec = showbPrec
{-# INLINE showbDualPrec #-}

-- | Convert a 'First' value to a 'Builder' with the given precedence.
showbFirstPrec :: Show a => Int -> First a -> Builder
showbFirstPrec = showbPrec
{-# INLINE showbFirstPrec #-}

-- | Convert a 'Last' value to a 'Builder' with the given precedence.
showbLastPrec :: Show a => Int -> Last a -> Builder
showbLastPrec = showbPrec
{-# INLINE showbLastPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given precedence.
showbProductPrec :: Show a => Int -> Product a -> Builder
showbProductPrec = showbPrec
{-# INLINE showbProductPrec #-}

-- | Convert a 'Sum' value to a 'Builder' with the given precedence.
showbSumPrec :: Show a => Int -> Sum a -> Builder
showbSumPrec = showbPrec
{-# INLINE showbSumPrec #-}

$(deriveShow ''All)
$(deriveShow ''Any)
$(deriveShow ''Dual)
$(deriveShow ''First)
$(deriveShow ''Last)
$(deriveShow ''Product)
$(deriveShow ''Sum)

instance Show1 Dual where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance Show1 First where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance Show1 Last where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance Show1 Product where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance Show1 Sum where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}