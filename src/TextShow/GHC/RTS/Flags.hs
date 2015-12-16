{-# LANGUAGE CPP               #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.RTS.Flags
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the 'GHC.RTS.Flags' module.
This module only exports functions if using @base-4.8.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.RTS.Flags (
#if !(MIN_VERSION_base(4,8,0))
    ) where
#else
      showbRTSFlagsPrec
    , showbGCFlagsPrec
    , showbConcFlagsPrec
    , showbMiscFlagsPrec
    , showbDebugFlagsPrec
    , showbCCFlagsPrec
    , showbProfFlagsPrec
    , showbTraceFlagsPrec
    , showbTickyFlagsPrec
# if MIN_VERSION_base(4,8,2)
    , showbGiveGCStats
    , showbDoCostCentres
    , showbDoHeapProfile
    , showbDoTrace
# endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.RTS.Flags

import TextShow.Classes (TextShow(..))
import TextShow.Data.Bool     ()
import TextShow.Data.Char     ()
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Maybe    ()
import TextShow.TH.Internal (deriveTextShow)
import TextShow.TH.Names (giveGCStatsTypeName, doCostCentresTypeName,
                          doHeapProfileTypeName, doTraceTypeName)

-- | Convert an 'RTSFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbRTSFlagsPrec :: Int -> RTSFlags -> Builder
showbRTSFlagsPrec = showbPrec
{-# INLINE showbRTSFlagsPrec #-}

-- | Convert a 'GCFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbGCFlagsPrec :: Int -> GCFlags -> Builder
showbGCFlagsPrec = showbPrec
{-# INLINE showbGCFlagsPrec #-}

-- | Convert a 'ConcFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbConcFlagsPrec :: Int -> ConcFlags -> Builder
showbConcFlagsPrec = showbPrec
{-# INLINE showbConcFlagsPrec #-}

-- | Convert a 'MiscFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbMiscFlagsPrec :: Int -> MiscFlags -> Builder
showbMiscFlagsPrec = showbPrec
{-# INLINE showbMiscFlagsPrec #-}

-- | Convert a 'DebugFlags' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbDebugFlagsPrec :: Int -> DebugFlags -> Builder
showbDebugFlagsPrec = showbPrec
{-# INLINE showbDebugFlagsPrec #-}

-- | Convert a 'CCFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbCCFlagsPrec :: Int -> CCFlags -> Builder
showbCCFlagsPrec = showbPrec
{-# INLINE showbCCFlagsPrec #-}

-- | Convert a 'ProfFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbProfFlagsPrec :: Int -> ProfFlags -> Builder
showbProfFlagsPrec = showbPrec
{-# INLINE showbProfFlagsPrec #-}

-- | Convert a 'TraceFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbTraceFlagsPrec :: Int -> TraceFlags -> Builder
showbTraceFlagsPrec = showbPrec
{-# INLINE showbTraceFlagsPrec #-}

-- | Convert a 'TickyFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbTickyFlagsPrec :: Int -> TickyFlags -> Builder
showbTickyFlagsPrec = showbPrec
{-# INLINE showbTickyFlagsPrec #-}

# if MIN_VERSION_base(4,8,2)
-- | Convert a 'GiveGCStats' value to a 'Builder'.
-- This function is only available with @base-4.8.2.0@ or later.
--
-- /Since: 2.1/
showbGiveGCStats :: GiveGCStats -> Builder
showbGiveGCStats = showb
{-# INLINE showbGiveGCStats #-}

-- | Convert a 'DoCostCentres' value to a 'Builder'.
-- This function is only available with @base-4.8.2.0@ or later.
--
-- /Since: 2.1/
showbDoCostCentres :: DoCostCentres -> Builder
showbDoCostCentres = showb
{-# INLINE showbDoCostCentres #-}

-- | Convert a 'DoHeapProfile' value to a 'Builder'.
-- This function is only available with @base-4.8.2.0@ or later.
--
-- /Since: 2.1/
showbDoHeapProfile :: DoHeapProfile -> Builder
showbDoHeapProfile = showb
{-# INLINE showbDoHeapProfile #-}

-- | Convert a 'DoTrace' value to a 'Builder'.
-- This function is only available with @base-4.8.2.0@ or later.
--
-- /Since: 2.1/
showbDoTrace :: DoTrace -> Builder
showbDoTrace = showb
{-# INLINE showbDoTrace #-}
# endif

$(deriveTextShow ''RTSFlags)
$(deriveTextShow ''GCFlags)
$(deriveTextShow ''ConcFlags)
$(deriveTextShow ''MiscFlags)
$(deriveTextShow ''DebugFlags)
$(deriveTextShow ''CCFlags)
$(deriveTextShow ''ProfFlags)
$(deriveTextShow ''TraceFlags)
$(deriveTextShow ''TickyFlags)

$(deriveTextShow giveGCStatsTypeName)
$(deriveTextShow doCostCentresTypeName)
$(deriveTextShow doHeapProfileTypeName)
$(deriveTextShow doTraceTypeName)
#endif
