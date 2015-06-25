{-# LANGUAGE CPP               #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Text.Show.Text.GHC.RTS.Flags
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for data types in the 'GHC.RTS.Flags' module.
This module only exports functions if using @base-4.8.0.0@ or later.

/Since: 0.5/
-}
module Text.Show.Text.GHC.RTS.Flags (
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
import Data.Text.Lazy.Builder (Builder)

import GHC.RTS.Flags

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), FromStringShow(..))
import Text.Show.Text.Data.Bool (showbBool)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Floating (showbDoublePrec)
import Text.Show.Text.Data.Integral (showbIntPrec, showbWord, showbWord64)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Data.Maybe (showbMaybePrecWith)
import Text.Show.Text.Utils (s)
import Text.Show.Text.TH.Internal (deriveShow)

# if __GLASGOW_HASKELL__ < 711
import GHC.Show (appPrec)
import Text.Show.Text.Classes (showbParen)
# endif

-- | Convert an 'RTSFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
showbRTSFlagsPrec :: Int -> RTSFlags -> Builder
showbRTSFlagsPrec = showbPrec
{-# INLINE showbRTSFlagsPrec #-}

-- | Convert a 'GCFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
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
    <> s '}'

-- | Convert a 'ConcFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
showbConcFlagsPrec :: Int -> ConcFlags -> Builder
showbConcFlagsPrec = showbPrec
{-# INLINE showbConcFlagsPrec #-}

-- | Convert a 'MiscFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
showbMiscFlagsPrec :: Int -> MiscFlags -> Builder
showbMiscFlagsPrec = showbPrec
{-# INLINE showbMiscFlagsPrec #-}

-- | Convert a 'DebugFlags' value to a 'Builder' with the given precedence.
--
-- /Since: 0.5/
showbDebugFlagsPrec :: Int -> DebugFlags -> Builder
showbDebugFlagsPrec = showbPrec
{-# INLINE showbDebugFlagsPrec #-}

-- | Convert a 'CCFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
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
    <> s '}'

-- | Convert a 'ProfFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
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
    <> s '}'

-- | Convert a 'TraceFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
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
    <> s '}'

-- | Convert a 'TickyFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.5/
showbTickyFlagsPrec :: Int -> TickyFlags -> Builder
showbTickyFlagsPrec = showbPrec
{-# INLINE showbTickyFlagsPrec #-}

$(deriveShow ''RTSFlags)

instance Show GCFlags where
    showbPrec = showbGCFlagsPrec
    {-# INLINE showbPrec #-}

$(deriveShow ''ConcFlags)

$(deriveShow ''MiscFlags)

$(deriveShow ''DebugFlags)

instance Show CCFlags where
    showbPrec = showbCCFlagsPrec
    {-# INLINE showbPrec #-}

instance Show ProfFlags where
    showbPrec = showbProfFlagsPrec
    {-# INLINE showbPrec #-}

instance Show TraceFlags where
    showbPrec = showbTraceFlagsPrec
    {-# INLINE showbPrec #-}

$(deriveShow ''TickyFlags)
#endif
