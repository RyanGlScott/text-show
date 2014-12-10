{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Tuple
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for tuple types.
-}
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

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (deriveShow)

-- | Converts @()@ into a 'Builder'.
showbUnit :: () -> Builder
-- showbUnit () = "()"
showbUnit = showb
{-# INLINE showbUnit #-}

-- | Converts a 2-tuple into a 'Builder'.
showb2Tuple :: (Show a, Show b) => (a, b) -> Builder
showb2Tuple = showb
{-# INLINE showb2Tuple #-}

-- | Converts a 3-tuple into a 'Builder'.
showb3Tuple :: (Show a, Show b, Show c) => (a, b, c) -> Builder
showb3Tuple = showb
{-# INLINE showb3Tuple #-}

-- | Converts a 4-tuple into a 'Builder'.
showb4Tuple :: (Show a, Show b, Show c, Show d) => (a, b, c, d) -> Builder
showb4Tuple = showb
{-# INLINE showb4Tuple #-}

-- | Converts a 5-tuple into a 'Builder'.
showb5Tuple :: (Show a, Show b, Show c, Show d, Show e) => (a, b, c, d, e) -> Builder
showb5Tuple = showb
{-# INLINE showb5Tuple #-}

-- | Converts a 6-tuple into a 'Builder'.
showb6Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f) => (a, b, c, d, e, f) -> Builder
showb6Tuple = showb
{-# INLINE showb6Tuple #-}

-- | Converts a 7-tuple into a 'Builder'.
showb7Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
            => (a, b, c, d, e, f, g) -> Builder
showb7Tuple = showb
{-# INLINE showb7Tuple #-}

-- | Converts an 8-tuple into a 'Builder'.
showb8Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
            => (a, b, c, d, e, f, g, h) -> Builder
showb8Tuple = showb
{-# INLINE showb8Tuple #-}

-- | Converts a 9-tuple into a 'Builder'.
showb9Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
            => (a, b, c, d, e, f, g, h, i) -> Builder
showb9Tuple = showb
{-# INLINE showb9Tuple #-}

-- | Converts a 10-tuple into a 'Builder'.
showb10Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j)
             => (a, b, c, d, e, f, g, h, i, j) -> Builder
showb10Tuple = showb
{-# INLINE showb10Tuple #-}

-- | Converts an 11-tuple into a 'Builder'.
showb11Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f,
                 Show g, Show h, Show i, Show j, Show k)
             => (a, b, c, d, e, f, g, h, i, j, k) -> Builder
showb11Tuple = showb
{-# INLINE showb11Tuple #-}

-- | Converts a 12-tuple into a 'Builder'.
showb12Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f,
                 Show g, Show h, Show i, Show j, Show k, Show l)
             => (a, b, c, d, e, f, g, h, i, j, k, l) -> Builder
showb12Tuple = showb
{-# INLINE showb12Tuple #-}

-- | Converts a 13-tuple into a 'Builder'.
showb13Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
                 Show h, Show i, Show j, Show k, Show l, Show m)
             => (a, b, c, d, e, f, g, h, i, j, k, l, m) -> Builder
showb13Tuple = showb
{-# INLINE showb13Tuple #-}

-- | Converts a 14-tuple into a 'Builder'.
showb14Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
                 Show h, Show i, Show j, Show k, Show l, Show m, Show n)
             => (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> Builder
showb14Tuple = showb
{-# INLINE showb14Tuple #-}

-- | Converts a 15-tuple into a 'Builder'.
showb15Tuple :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
                 Show i, Show j, Show k, Show l, Show m, Show n, Show o)
             => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> Builder
showb15Tuple = showb
{-# INLINE showb15Tuple #-}

$(deriveShow ''())
$(deriveShow ''(,))
$(deriveShow ''(,,))
$(deriveShow ''(,,,))
$(deriveShow ''(,,,,))
$(deriveShow ''(,,,,,))
$(deriveShow ''(,,,,,,))
$(deriveShow ''(,,,,,,,))
$(deriveShow ''(,,,,,,,,))
$(deriveShow ''(,,,,,,,,,))
$(deriveShow ''(,,,,,,,,,,))
$(deriveShow ''(,,,,,,,,,,,))
$(deriveShow ''(,,,,,,,,,,,,))
$(deriveShow ''(,,,,,,,,,,,,,))
$(deriveShow ''(,,,,,,,,,,,,,,))

instance Show a => Show1 ((,) a) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b) => Show1 ((,,) a b) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c) => Show1 ((,,,) a b c) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d) => Show1 ((,,,,) a b c d) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e) => Show1 ((,,,,,) a b c d e) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show1 ((,,,,,,) a b c d e f) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show1 ((,,,,,,,) a b c d e f g) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) =>
  Show1 ((,,,,,,,,) a b c d e f g h) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) =>
  Show1 ((,,,,,,,,,) a b c d e f g h i) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) =>
  Show1 ((,,,,,,,,,,) a b c d e f g h i j) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) =>
  Show1 ((,,,,,,,,,,,) a b c d e f g h i j k) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) =>
  Show1 ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) =>
  Show1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
  Show1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}