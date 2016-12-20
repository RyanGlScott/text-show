{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
#endif
{-|
Module:      TextShow.GHC.Stats
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'GCStats'.
This module only exports functions if using @base-4.5.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.Stats (
#if !(MIN_VERSION_base(4,5,0))
    ) where
#else
      showbGCStatsPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Stats (GCStats)

import TextShow.Classes (showbPrec)
import TextShow.Data.Integral ()
import TextShow.Data.Floating ()
import TextShow.TH.Internal (deriveTextShow)

-- | Convert a 'GCStats' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.5.0.0@ or later.
--
-- /Since: 2/
showbGCStatsPrec :: Int -> GCStats -> Builder
showbGCStatsPrec = showbPrec
{-# INLINE showbGCStatsPrec #-}

$(deriveTextShow ''GCStats)
#endif
