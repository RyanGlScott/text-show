{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Tuple
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for tuple types.

/Since: 2/
-}
module TextShow.Data.Tuple (
      showbUnit
    , showb2TupleWith2
    , showb3TupleWith2
    , showb4TupleWith2
    , showb5TupleWith2
    , showb6TupleWith2
    , showb7TupleWith2
    , showb8TupleWith2
    , showb9TupleWith2
    , showb10TupleWith2
    , showb11TupleWith2
    , showb12TupleWith2
    , showb13TupleWith2
    , showb14TupleWith2
    , showb15TupleWith2
    ) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow2(..))
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

#include "inline.h"

-- | Converts @()@ into a 'Builder'.
--
-- /Since: 2/
showbUnit :: () -> Builder
-- showbUnit () = "()"
showbUnit = showb
{-# INLINE showbUnit #-}

-- | Converts a 2-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb2TupleWith2 :: (a -> Builder) -> (b -> Builder)
                 -> (a, b) -> Builder
showb2TupleWith2 = showbWith2
{-# INLINE showb2TupleWith2 #-}

-- | Converts a 3-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb3TupleWith2 :: TextShow a
                 => (b -> Builder) -> (c -> Builder)
                 -> (a, b, c) -> Builder
showb3TupleWith2 = showbWith2
{-# INLINE showb3TupleWith2 #-}

-- | Converts a 4-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb4TupleWith2 :: (TextShow a, TextShow b)
                 => (c -> Builder) -> (d -> Builder)
                 -> (a, b, c, d) -> Builder
showb4TupleWith2 = showbWith2
{-# INLINE showb4TupleWith2 #-}

-- | Converts a 5-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb5TupleWith2 :: (TextShow a, TextShow b, TextShow c)
                 => (d -> Builder) -> (e -> Builder)
                 -> (a, b, c, d, e) -> Builder
showb5TupleWith2 = showbWith2
{-# INLINE showb5TupleWith2 #-}

-- | Converts a 6-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb6TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d)
                 => (e -> Builder) -> (f -> Builder)
                 -> (a, b, c, d, e, f) -> Builder
showb6TupleWith2 = showbWith2
{-# INLINE showb6TupleWith2 #-}

-- | Converts a 7-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb7TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e)
                 => (f -> Builder) -> (g -> Builder)
                 -> (a, b, c, d, e, f, g) -> Builder
showb7TupleWith2 = showbWith2
{-# INLINE showb7TupleWith2 #-}

-- | Converts an 8-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb8TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f)
                 => (g -> Builder) -> (h -> Builder)
                 -> (a, b, c, d, e, f, g, h) -> Builder
showb8TupleWith2 = showbWith2
{-# INLINE showb8TupleWith2 #-}

-- | Converts a 9-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb9TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g)
                 => (h -> Builder) -> (i -> Builder)
                 -> (a, b, c, d, e, f, g, h, i) -> Builder
showb9TupleWith2 = showbWith2
{-# INLINE showb9TupleWith2 #-}

-- | Converts a 10-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb10TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g, TextShow h)
                  => (i -> Builder) -> (j -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j) -> Builder
showb10TupleWith2 = showbWith2
{-# INLINE showb10TupleWith2 #-}

-- | Converts an 11-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb11TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e,
                      TextShow f, TextShow g, TextShow h, TextShow i)
                  => (j -> Builder) -> (k -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k) -> Builder
showb11TupleWith2 = showbWith2
{-# INLINE showb11TupleWith2 #-}

-- | Converts a 12-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb12TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e,
                      TextShow f, TextShow g, TextShow h, TextShow i, TextShow j)
                  => (k -> Builder) -> (l -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l) -> Builder
showb12TupleWith2 = showbWith2
{-# INLINE showb12TupleWith2 #-}

-- | Converts a 13-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb13TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f,
                      TextShow g, TextShow h, TextShow i, TextShow j, TextShow k)
                  => (l -> Builder) -> (m -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l, m) -> Builder
showb13TupleWith2 = showbWith2
{-# INLINE showb13TupleWith2 #-}

-- | Converts a 14-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb14TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f,
                      TextShow g, TextShow h, TextShow i, TextShow j, TextShow k, TextShow l)
                  => (m -> Builder) -> (n -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> Builder
showb14TupleWith2 = showbWith2
{-# INLINE showb14TupleWith2 #-}

-- | Converts a 15-tuple into a 'Builder' with the given show functions.
--
-- /Since: 2/
showb15TupleWith2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g,
                      TextShow h, TextShow i, TextShow j, TextShow k, TextShow l, TextShow m)
                  => (n -> Builder) -> (o -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> Builder
showb15TupleWith2 = showbWith2
{-# INLINE showb15TupleWith2 #-}

-- | Like 'showbPrecWith2', except precedence-agnostic.
showbWith2 :: TextShow2 f => (a -> Builder) -> (b -> Builder) -> f a b -> Builder
showbWith2 sp1 sp2 = showbPrecWith2 (const sp1) (const sp2) 0
{-# INLINE showbWith2 #-}

-- The Great Pyramids of Template Haskell
$(deriveTextShow ''())
$(deriveTextShow ''(,))
$(deriveTextShow ''(,,))
$(deriveTextShow ''(,,,))
$(deriveTextShow ''(,,,,))
$(deriveTextShow ''(,,,,,))
$(deriveTextShow ''(,,,,,,))
$(deriveTextShow ''(,,,,,,,))
$(deriveTextShow ''(,,,,,,,,))
$(deriveTextShow ''(,,,,,,,,,))
$(deriveTextShow ''(,,,,,,,,,,))
$(deriveTextShow ''(,,,,,,,,,,,))
$(deriveTextShow ''(,,,,,,,,,,,,))
$(deriveTextShow ''(,,,,,,,,,,,,,))
$(deriveTextShow ''(,,,,,,,,,,,,,,))

$(deriveTextShow1 ''(,))
$(deriveTextShow1 ''(,,))
$(deriveTextShow1 ''(,,,))
$(deriveTextShow1 ''(,,,,))
$(deriveTextShow1 ''(,,,,,))
$(deriveTextShow1 ''(,,,,,,))
$(deriveTextShow1 ''(,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,,,,,,))
$(deriveTextShow1 ''(,,,,,,,,,,,,,,))

$(deriveTextShow2 ''(,))
$(deriveTextShow2 ''(,,))
$(deriveTextShow2 ''(,,,))
$(deriveTextShow2 ''(,,,,))
$(deriveTextShow2 ''(,,,,,))
$(deriveTextShow2 ''(,,,,,,))
$(deriveTextShow2 ''(,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,,,,,,))
$(deriveTextShow2 ''(,,,,,,,,,,,,,,))
