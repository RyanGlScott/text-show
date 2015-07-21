{-# LANGUAGE CPP               #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, singleton)

import GHC.RTS.Flags

import TextShow.Classes (TextShow(showb, showbPrec))
import TextShow.Data.Bool (showbBool)
import TextShow.Data.Char ()
import TextShow.Data.Floating (showbDoublePrec)
import TextShow.Data.Integral (showbIntPrec, showbWord, showbWord64)
import TextShow.Data.List ()
import TextShow.Data.Maybe (showbMaybePrecWith)
import TextShow.FromStringTextShow (FromStringShow(..))
import TextShow.TH.Internal (deriveTextShow)

# if __GLASGOW_HASKELL__ < 711
import GHC.Show (appPrec)
import TextShow.Classes (showbParen)
# endif

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
# if __GLASGOW_HASKELL__ >= 711
showbGCFlagsPrec _ gcfs =
# else
showbGCFlagsPrec p gcfs = showbParen (p > appPrec) $
# endif
       "GCFlags {statsFile = "
    <> showbMaybePrecWith showbPrec 0 (statsFile gcfs)
    <> ", giveStats = "
    <> showb (FromStringShow $ giveStats gcfs)
    <> ", maxStkSize = "
    <> showb (maxStkSize gcfs)
    <> ", initialStkSize = "
    <> showb (initialStkSize gcfs)
    <> ", stkChunkSize = "
    <> showb (stkChunkSize gcfs)
    <> ", stkChunkBufferSize = "
    <> showb (stkChunkBufferSize gcfs)
    <> ", maxHeapSize = "
    <> showb (maxHeapSize gcfs)
    <> ", minAllocAreaSize = "
    <> showb (minAllocAreaSize gcfs)
    <> ", minOldGenSize = "
    <> showb (minOldGenSize gcfs)
    <> ", heapSizeSuggestion = "
    <> showb (heapSizeSuggestion gcfs)
    <> ", heapSizeSuggestionAuto = "
    <> showbBool (heapSizeSuggestionAuto gcfs)
    <> ", oldGenFactor = "
    <> showbDoublePrec 0 (oldGenFactor gcfs)
    <> ", pcFreeHeap = "
    <> showbDoublePrec 0 (pcFreeHeap gcfs)
    <> ", generations = "
    <> showb (generations gcfs)
    <> ", steps = "
    <> showb (steps gcfs)
    <> ", squeezeUpdFrames = "
    <> showbBool (squeezeUpdFrames gcfs)
    <> ", compact = "
    <> showbBool (compact gcfs)
    <> ", compactThreshold = "
    <> showbDoublePrec 0 (compactThreshold gcfs)
    <> ", sweep = "
    <> showbBool (sweep gcfs)
    <> ", ringBell = "
    <> showbBool (ringBell gcfs)
    <> ", frontpanel = "
    <> showbBool (frontpanel gcfs)
    <> ", idleGCDelayTime = "
    <> showbWord64 (idleGCDelayTime gcfs)
    <> ", doIdleGC = "
    <> showbBool (doIdleGC gcfs)
    <> ", heapBase = "
    <> showbWord (heapBase gcfs)
    <> ", allocLimitGrace = "
    <> showbWord (allocLimitGrace gcfs)
    <> singleton '}'

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
# if __GLASGOW_HASKELL__ >= 711
showbCCFlagsPrec _ ccfs =
# else
showbCCFlagsPrec p ccfs = showbParen (p > appPrec) $
# endif
       "CCFlags {doCostCentres = "
    <> showb (FromStringShow $ doCostCentres ccfs)
    <> ", profilerTicks = "
    <> showbIntPrec 0 (profilerTicks ccfs)
    <> ", msecsPerTick = "
    <> showbIntPrec 0 (msecsPerTick ccfs)
    <> singleton '}'

-- | Convert a 'ProfFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbProfFlagsPrec :: Int -> ProfFlags -> Builder
# if __GLASGOW_HASKELL__ >= 711
showbProfFlagsPrec _ pfs =
# else
showbProfFlagsPrec p pfs = showbParen (p > appPrec) $
# endif
       "ProfFlags {doHeapProfile = "
    <> showb (FromStringShow $ doHeapProfile pfs)
    <> ", heapProfileInterval = "
    <> showbWord64 (heapProfileInterval pfs)
    <> ", heapProfileIntervalTicks = "
    <> showbWord (heapProfileIntervalTicks pfs)
    <> ", includeTSOs = "
    <> showbBool (includeTSOs pfs)
    <> ", showCCSOnException = "
    <> showbBool (showCCSOnException pfs)
    <> ", maxRetainerSetSize = "
    <> showbWord (maxRetainerSetSize pfs)
    <> ", ccsLength = "
    <> showbWord (ccsLength pfs)
    <> ", modSelector = "
    <> showbMaybePrecWith showbPrec 0 (modSelector pfs)
    <> ", descrSelector = "
    <> showbMaybePrecWith showbPrec 0 (descrSelector pfs)
    <> ", typeSelector = "
    <> showbMaybePrecWith showbPrec 0 (typeSelector pfs)
    <> ", ccSelector = "
    <> showbMaybePrecWith showbPrec 0 (ccSelector pfs)
    <> ", ccsSelector = "
    <> showbMaybePrecWith showbPrec 0 (ccsSelector pfs)
    <> ", retainerSelector = "
    <> showbMaybePrecWith showbPrec 0 (retainerSelector pfs)
    <> ", bioSelector = "
    <> showbMaybePrecWith showbPrec 0 (bioSelector pfs)
    <> singleton '}'

-- | Convert a 'TraceFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbTraceFlagsPrec :: Int -> TraceFlags -> Builder
# if __GLASGOW_HASKELL__ >= 711
showbTraceFlagsPrec _ tfs =
# else
showbTraceFlagsPrec p tfs = showbParen (p > appPrec) $
# endif
       "TraceFlags {tracing = "
    <> showb (FromStringShow $ tracing tfs)
    <> ", timestamp = "
    <> showbBool (timestamp tfs)
    <> ", traceScheduler = "
    <> showbBool (traceScheduler tfs)
    <> ", traceGc = "
    <> showbBool (traceGc tfs)
    <> ", sparksSampled = "
    <> showbBool (sparksSampled tfs)
    <> ", sparksFull = "
    <> showbBool (sparksFull tfs)
    <> ", user = "
    <> showbBool (user tfs)
    <> singleton '}'

-- | Convert a 'TickyFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbTickyFlagsPrec :: Int -> TickyFlags -> Builder
showbTickyFlagsPrec = showbPrec
{-# INLINE showbTickyFlagsPrec #-}

$(deriveTextShow ''RTSFlags)

instance TextShow GCFlags where
    showbPrec = showbGCFlagsPrec
    {-# INLINE showbPrec #-}

$(deriveTextShow ''ConcFlags)

$(deriveTextShow ''MiscFlags)

$(deriveTextShow ''DebugFlags)

instance TextShow CCFlags where
    showbPrec = showbCCFlagsPrec
    {-# INLINE showbPrec #-}

instance TextShow ProfFlags where
    showbPrec = showbProfFlagsPrec
    {-# INLINE showbPrec #-}

instance TextShow TraceFlags where
    showbPrec = showbTraceFlagsPrec
    {-# INLINE showbPrec #-}

$(deriveTextShow ''TickyFlags)
#endif
