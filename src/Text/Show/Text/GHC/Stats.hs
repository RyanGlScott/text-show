{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.Stats
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'GCStats'.
This module is only available with @base-4.5.0.0@ or later.

/Since: 0.3/
-}
module Text.Show.Text.GHC.Stats (showbGCStatsPrec) where 

import Data.Text.Lazy.Builder (Builder)

import GHC.Stats (GCStats)

import Text.Show.Text.Classes (showbPrec)
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert a 'GCStats' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.5.0.0@ or later.
-- 
-- /Since: 0.3/
showbGCStatsPrec :: Int -> GCStats -> Builder
showbGCStatsPrec = showbPrec
{-# INLINE showbGCStatsPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''GCStats)