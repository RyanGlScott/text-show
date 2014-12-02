{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Tuple
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for tuple types.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Tuple (
      showbUnit
    , showb2Tuple
    , showb3Tuple
    , showb4Tuple
    , showb5Tuple
    , showb6Tuple
    , showb7Tuple
    , showb8Tuple
    , showb9Tuple
    , showb10Tuple
    , showb11Tuple
    , showb12Tuple
    , showb13Tuple
    , showb14Tuple
    , showb15Tuple
    ) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb))
import Text.Show.Text.Utils (s)

-- | Converts @()@ into a 'Builder'.
showbUnit :: () -> Builder
showbUnit () = "()"
{-# INLINE showbUnit #-}

-- | Converts a 2-tuple into a 'Builder'.
showb2Tuple :: (Show a, Show b) => (a, b) -> Builder
showb2Tuple (a, b) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ')'
{-# INLINE showb2Tuple #-}

-- | Converts a 3-tuple into a 'Builder'.
showb3Tuple :: (Show a, Show b, Show c) => (a, b, c) -> Builder
showb3Tuple (a, b, c) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ')'
{-# INLINE showb3Tuple #-}

-- | Converts a 4-tuple into a 'Builder'.
showb4Tuple :: (Show a, Show b, Show c, Show d) => (a, b, c, d) -> Builder
showb4Tuple (a, b, c, d) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ')'
{-# INLINE showb4Tuple #-}

-- | Converts a 5-tuple into a 'Builder'.
showb5Tuple :: (Show a, Show b, Show c, Show d, Show e) => (a, b, c, d, e) -> Builder
showb5Tuple (a, b, c, d, e) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ')'
{-# INLINE showb5Tuple #-}

-- | Converts a 6-tuple into a 'Builder'.
showb6Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f) => (a, b, c, d, e, f) -> Builder
showb6Tuple (a, b, c, d, e, f) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ')'
{-# INLINE showb6Tuple #-}

-- | Converts a 7-tuple into a 'Builder'.
showb7Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
            => (a, b, c, d, e, f, g) -> Builder
showb7Tuple (a, b, c, d, e, f, g) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ')'
{-# INLINE showb7Tuple #-}

-- | Converts an 8-tuple into a 'Builder'.
showb8Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
            => (a, b, c, d, e, f, g, h) -> Builder
showb8Tuple (a, b, c, d, e, f, g, h) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ')'
{-# INLINE showb8Tuple #-}

-- | Converts a 9-tuple into a 'Builder'.
showb9Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
            => (a, b, c, d, e, f, g, h, i) -> Builder
showb9Tuple (a, b, c, d, e, f, g, h, i) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ')'
{-# INLINE showb9Tuple #-}

-- | Converts a 10-tuple into a 'Builder'.
showb10Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j)
             => (a, b, c, d, e, f, g, h, i, j) -> Builder
showb10Tuple (a, b, c, d, e, f, g, h, i, j) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ',' <> showb j <>
    s ')'
{-# INLINE showb10Tuple #-}

-- | Converts an 11-tuple into a 'Builder'.
showb11Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f,
                 Show g, Show h, Show i, Show j, Show k)
             => (a, b, c, d, e, f, g, h, i, j, k) -> Builder
showb11Tuple (a, b, c, d, e, f, g, h, i, j, k) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ',' <> showb j <>
    s ',' <> showb k <>
    s ')'
{-# INLINE showb11Tuple #-}

-- | Converts a 12-tuple into a 'Builder'.
showb12Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f,
                 Show g, Show h, Show i, Show j, Show k, Show l)
             => (a, b, c, d, e, f, g, h, i, j, k, l) -> Builder
showb12Tuple (a, b, c, d, e, f, g, h, i, j, k, l) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ',' <> showb j <>
    s ',' <> showb k <>
    s ',' <> showb l <>
    s ')'
{-# INLINE showb12Tuple #-}

-- | Converts a 13-tuple into a 'Builder'.
showb13Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
                 Show h, Show i, Show j, Show k, Show l, Show m)
             => (a, b, c, d, e, f, g, h, i, j, k, l, m) -> Builder
showb13Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ',' <> showb j <>
    s ',' <> showb k <>
    s ',' <> showb l <>
    s ',' <> showb m <>
    s ')'
{-# INLINE showb13Tuple #-}

-- | Converts a 14-tuple into a 'Builder'.
showb14Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
                 Show h, Show i, Show j, Show k, Show l, Show m, Show n)
             => (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> Builder
showb14Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ',' <> showb j <>
    s ',' <> showb k <>
    s ',' <> showb l <>
    s ',' <> showb m <>
    s ',' <> showb n <>
    s ')'
{-# INLINE showb14Tuple #-}

-- | Converts a 15-tuple into a 'Builder'.
showb15Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
                 Show i, Show j, Show k, Show l, Show m, Show n, Show o)
             => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> Builder
showb15Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
    s '(' <> showb a <>
    s ',' <> showb b <>
    s ',' <> showb c <>
    s ',' <> showb d <>
    s ',' <> showb e <>
    s ',' <> showb f <>
    s ',' <> showb g <>
    s ',' <> showb h <>
    s ',' <> showb i <>
    s ',' <> showb j <>
    s ',' <> showb k <>
    s ',' <> showb l <>
    s ',' <> showb m <>
    s ',' <> showb n <>
    s ',' <> showb o <>
    s ')'
{-# INLINE showb15Tuple #-}

instance Show () where
    showb = showbUnit
    {-# INLINE showb #-}

instance (Show a, Show b) => Show (a, b) where
    showb = showb2Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showb = showb3Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showb = showb4Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showb = showb5Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
    showb = showb6Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) =>
  Show (a, b, c, d, e, f, g) where
    showb = showb7Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) =>
  Show (a, b, c, d, e, f, g, h) where
    showb = showb8Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) =>
  Show (a, b, c, d, e, f, g, h, i) where
    showb = showb9Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) =>
  Show (a, b, c, d, e, f, g, h, i, j) where
    showb = showb10Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f,
          Show g, Show h, Show i, Show j, Show k) =>
  Show (a, b, c, d, e, f, g, h, i, j, k) where
    showb = showb11Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f,
          Show g, Show h, Show i, Show j, Show k, Show l) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l) where
    showb = showb12Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    showb = showb13Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    showb = showb14Tuple
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    showb = showb15Tuple
    {-# INLINE showb #-}