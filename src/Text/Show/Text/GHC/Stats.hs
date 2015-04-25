{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Text.Show.Text.GHC.Stats
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'GCStats'.
This module only exports functions if using @base-4.5.0.0@ or later.

/Since: 0.3/
-}
module Text.Show.Text.GHC.Stats (
#if !(MIN_VERSION_base(4,5,0))
    ) where
#else
      showbGCStatsPrec
    ) where 

import Data.Text.Lazy.Builder (Builder)

import GHC.Stats (GCStats)

import Text.Show.Text.Classes (showbPrec)
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.TH.Internal (deriveShow)

-- | Convert a 'GCStats' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.5.0.0@ or later.
-- 
-- /Since: 0.3/
showbGCStatsPrec :: Int -> GCStats -> Builder
showbGCStatsPrec = showbPrec
{-# INLINE showbGCStatsPrec #-}

$(deriveShow ''GCStats)
#endif
