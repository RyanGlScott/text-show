{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ord
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Ordering' and 'Down'.

/Since: 0.3/
-}
module Text.Show.Text.Data.Ord (
      showbOrdering
#if MIN_VERSION_base(4,6,0)
    , showbDownPrec
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import Text.Show.Text.Classes (showb)
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb)

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (defaultInlineShowbPrec)
#endif

#include "inline.h"

-- | Convert a 'Ordering' to a 'Builder'.
-- 
-- /Since: 0.3/
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

$(deriveShowPragmas defaultInlineShowb ''Ordering)

#if MIN_VERSION_base(4,6,0)
-- | Convert a 'Down' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.6.0.0@ or later.
-- 
-- /Since: 0.3/
showbDownPrec :: Show a => Int -> Down a -> Builder
showbDownPrec = showbPrec
{-# INLINE showbDownPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Down)

instance Show1 Down where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}
#endif