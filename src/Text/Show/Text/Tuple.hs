{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Tuple where

import Data.Monoid ((<>))
import Prelude hiding (Show)
import Text.Show.Text.Class (Show(showb))
import Text.Show.Text.Functions (s)

instance (Show a, Show b) => Show (a, b) where
    showb (a, b) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showb (a, b, c) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showb (a, b, c, d) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showb (a, b, c, d, e) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
    showb (a, b, c, d, e, f) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) =>
  Show (a, b, c, d, e, f, g) where
    showb (a, b, c, d, e, f, g) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) =>
  Show (a, b, c, d, e, f, g, h) where
    showb (a, b, c, d, e, f, g, h) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ')'
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) =>
  Show (a, b, c, d, e, f, g, h, i) where
    showb (a, b, c, d, e, f, g, h, i) =
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
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) =>
  Show (a, b, c, d, e, f, g, h, i, j) where
    showb (a, b, c, d, e, f, g, h, i, j) =
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
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f,
          Show g, Show h, Show i, Show j, Show k) =>
  Show (a, b, c, d, e, f, g, h, i, j, k) where
    showb (a, b, c, d, e, f, g, h, i, j, k) =
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
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f,
          Show g, Show h, Show i, Show j, Show k, Show l) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l) =
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
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l, m) =
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
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
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
    {-# INLINE showb #-}

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
  Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
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
    {-# INLINE showb #-}