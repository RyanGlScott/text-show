{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ord
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for 'Ordering' and 'Down'.

/Since: 0.3/
-}
module Text.Show.Text.Data.Ord (
      showbOrdering
#if MIN_VERSION_base(4,6,0)
    , showbDownPrecWith
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (showb)
import Text.Show.Text.TH.Internal (deriveShow)

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)

import Prelude hiding (Show)

import Text.Show.Text.Classes (showbPrecWith)
import Text.Show.Text.TH.Internal (deriveShow1)
#endif

#include "inline.h"

-- | Convert a 'Ordering' to a 'Builder'.
--
-- /Since: 0.3/
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

#if MIN_VERSION_base(4,6,0)
-- | Convert a 'Down' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.6.0.0@ or later.
--
-- /Since: 1/
showbDownPrecWith :: (Int -> a -> Builder) -> Int -> Down a -> Builder
showbDownPrecWith = showbPrecWith
{-# INLINE showbDownPrecWith #-}
#endif

$(deriveShow  ''Ordering)

#if MIN_VERSION_base(4,6,0)
$(deriveShow  ''Down)
$(deriveShow1 ''Down)
#endif
