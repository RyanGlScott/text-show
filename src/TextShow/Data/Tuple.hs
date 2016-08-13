{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Tuple
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for tuple types.

/Since: 2/
-}
module TextShow.Data.Tuple (
      showbUnit
    , liftShowb2Tuple2
    , liftShowb3Tuple2
    , liftShowb4Tuple2
    , liftShowb5Tuple2
    , liftShowb6Tuple2
    , liftShowb7Tuple2
    , liftShowb8Tuple2
    , liftShowb9Tuple2
    , liftShowb10Tuple2
    , liftShowb11Tuple2
    , liftShowb12Tuple2
    , liftShowb13Tuple2
    , liftShowb14Tuple2
    , liftShowb15Tuple2
    ) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow2(..))
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

#include "inline.h"

-- | Converts @()@ into a 'Builder'.
--
-- /Since: 2/
showbUnit :: () -> Builder
showbUnit = showb

-- | Converts a 2-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb2Tuple2 :: (a -> Builder) -> (b -> Builder)
                 -> (a, b) -> Builder
liftShowb2Tuple2 = liftShowb2
{-# INLINE liftShowb2Tuple2 #-}

-- | Converts a 3-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb3Tuple2 :: TextShow a
                 => (b -> Builder) -> (c -> Builder)
                 -> (a, b, c) -> Builder
liftShowb3Tuple2 = liftShowb2
{-# INLINE liftShowb3Tuple2 #-}

-- | Converts a 4-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb4Tuple2 :: (TextShow a, TextShow b)
                 => (c -> Builder) -> (d -> Builder)
                 -> (a, b, c, d) -> Builder
liftShowb4Tuple2 = liftShowb2
{-# INLINE liftShowb4Tuple2 #-}

-- | Converts a 5-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb5Tuple2 :: (TextShow a, TextShow b, TextShow c)
                 => (d -> Builder) -> (e -> Builder)
                 -> (a, b, c, d, e) -> Builder
liftShowb5Tuple2 = liftShowb2
{-# INLINE liftShowb5Tuple2 #-}

-- | Converts a 6-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb6Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d)
                 => (e -> Builder) -> (f -> Builder)
                 -> (a, b, c, d, e, f) -> Builder
liftShowb6Tuple2 = liftShowb2
{-# INLINE liftShowb6Tuple2 #-}

-- | Converts a 7-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb7Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e)
                 => (f -> Builder) -> (g -> Builder)
                 -> (a, b, c, d, e, f, g) -> Builder
liftShowb7Tuple2 = liftShowb2
{-# INLINE liftShowb7Tuple2 #-}

-- | Converts an 8-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb8Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f)
                 => (g -> Builder) -> (h -> Builder)
                 -> (a, b, c, d, e, f, g, h) -> Builder
liftShowb8Tuple2 = liftShowb2
{-# INLINE liftShowb8Tuple2 #-}

-- | Converts a 9-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb9Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g)
                 => (h -> Builder) -> (i -> Builder)
                 -> (a, b, c, d, e, f, g, h, i) -> Builder
liftShowb9Tuple2 = liftShowb2
{-# INLINE liftShowb9Tuple2 #-}

-- | Converts a 10-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb10Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g, TextShow h)
                  => (i -> Builder) -> (j -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j) -> Builder
liftShowb10Tuple2 = liftShowb2
{-# INLINE liftShowb10Tuple2 #-}

-- | Converts an 11-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb11Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e,
                      TextShow f, TextShow g, TextShow h, TextShow i)
                  => (j -> Builder) -> (k -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k) -> Builder
liftShowb11Tuple2 = liftShowb2
{-# INLINE liftShowb11Tuple2 #-}

-- | Converts a 12-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb12Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e,
                      TextShow f, TextShow g, TextShow h, TextShow i, TextShow j)
                  => (k -> Builder) -> (l -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l) -> Builder
liftShowb12Tuple2 = liftShowb2
{-# INLINE liftShowb12Tuple2 #-}

-- | Converts a 13-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb13Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f,
                      TextShow g, TextShow h, TextShow i, TextShow j, TextShow k)
                  => (l -> Builder) -> (m -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l, m) -> Builder
liftShowb13Tuple2 = liftShowb2
{-# INLINE liftShowb13Tuple2 #-}

-- | Converts a 14-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb14Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f,
                      TextShow g, TextShow h, TextShow i, TextShow j, TextShow k, TextShow l)
                  => (m -> Builder) -> (n -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> Builder
liftShowb14Tuple2 = liftShowb2
{-# INLINE liftShowb14Tuple2 #-}

-- | Converts a 15-tuple into a 'Builder' with the given show functions.
--
-- /Since: 3/
liftShowb15Tuple2 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g,
                      TextShow h, TextShow i, TextShow j, TextShow k, TextShow l, TextShow m)
                  => (n -> Builder) -> (o -> Builder)
                  -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> Builder
liftShowb15Tuple2 = liftShowb2
{-# INLINE liftShowb15Tuple2 #-}

-- | Like 'liftShowbPrec2', except precedence-agnostic.
liftShowb2 :: TextShow2 f => (a -> Builder) -> (b -> Builder) -> f a b -> Builder
liftShowb2 sp1 sp2 = liftShowbPrec2 (const sp1) undefined (const sp2) undefined 0

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
