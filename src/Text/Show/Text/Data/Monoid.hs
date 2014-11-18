{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Monoid
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'Monoid'-related newtypes.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Monoid (
      showbAllPrec
    , showbAnyPrec
    , showbDualPrec
    , showbFirstPrec
    , showbLastPrec
    , showbProductPrec
    , showbSumPrec
    ) where

import Data.Monoid (All(..), Any(..), Dual(..), First(..),
                    Last(..), Product(..), Sum(..), (<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.Bool (showbBool)
import Text.Show.Text.Data.Maybe (showbMaybePrec)
import Text.Show.Text.Utils (s)

-- | Convert an 'All' value to a 'Builder' with the given precedence.
showbAllPrec :: Int -> All -> Builder
showbAllPrec p (All a) = showbParen (p > appPrec) $
        "All {getAll = "
     <> showbBool a
     <> s '}'
{-# INLINE showbAllPrec #-}

-- | Convert an 'Any' value to a 'Builder' with the given precedence.
showbAnyPrec :: Int -> Any -> Builder
showbAnyPrec p (Any a) = showbParen (p > appPrec) $
        "Any {getAny = "
     <> showbBool a
     <> s '}'
{-# INLINE showbAnyPrec #-}

-- | Convert a 'Dual' value to a 'Builder' with the given precedence.
showbDualPrec :: Show a => Int -> Dual a -> Builder
showbDualPrec p (Dual d) = showbParen (p > appPrec) $
        "Dual {getDual = "
     <> showb d
     <> s '}'
{-# INLINE showbDualPrec #-}

-- | Convert a 'First' value to a 'Builder' with the given precedence.
showbFirstPrec :: Show a => Int -> First a -> Builder
showbFirstPrec p (First f) = showbParen (p > appPrec) $
        "First {getFirst = "
     <> showbMaybePrec 0 f
     <> s '}'
{-# INLINE showbFirstPrec #-}

-- | Convert a 'Last' value to a 'Builder' with the given precedence.
showbLastPrec :: Show a => Int -> Last a -> Builder
showbLastPrec p (Last l) = showbParen (p > appPrec) $
        "Last {getLast = "
     <> showbMaybePrec 0 l
     <> s '}'
{-# INLINE showbLastPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given precedence.
showbProductPrec :: Show a => Int -> Product a -> Builder
showbProductPrec p (Product prod) = showbParen (p > appPrec) $
        "Product {getProduct = "
     <> showb prod
     <> s '}'
{-# INLINE showbProductPrec #-}

-- | Convert a 'Sum' value to a 'Builder' with the given precedence.
showbSumPrec :: Show a => Int -> Sum a -> Builder
showbSumPrec p (Sum sum') = showbParen (p > appPrec) $
        "Sum {getSum = "
     <> showb sum'
     <> s '}'
{-# INLINE showbSumPrec #-}

instance Show All where
    showbPrec = showbAllPrec
    {-# INLINE showbPrec #-}

instance Show Any where
    showbPrec = showbAnyPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Dual a) where
    showbPrec = showbDualPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (First a) where
    showbPrec = showbFirstPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Last a) where
    showbPrec = showbLastPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Product a) where
    showbPrec = showbProductPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Sum a) where
    showbPrec = showbSumPrec
    {-# INLINE showbPrec #-}