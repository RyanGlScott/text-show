{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.RTS.Flags
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the 'GHC.RTS.Flags' module.
This module is only available with @base-4.8.0.0@ or later.

/Since: 0.5/
-}
module Text.Show.Text.GHC.RTS.Flags (
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

import Data.Text.Lazy.Builder (Builder)

import GHC.RTS.Flags
import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), showbParen, FromStringShow(..))
import Text.Show.Text.Data.Bool (showbBool)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Floating (showbDoublePrec)
import Text.Show.Text.Data.Integral (showbIntPrec, showbWord, showbWord64)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Data.Maybe (showbMaybePrec)
import Text.Show.Text.Utils ((<>), s)
import Text.Show.Text.TH.Internal (deriveShow)

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
showbGCFlagsPrec p gcfs = showbParen (p > appPrec) $
       "GCFlags {statsFile = "
    <> showbMaybePrec 0 (statsFile gcfs)
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
    <> ", heapSizeSuggesionAuto = "
    <> showbBool (heapSizeSuggesionAuto gcfs)
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
showbCCFlagsPrec p ccfs = showbParen (p > appPrec) $
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
showbProfFlagsPrec p pfs = showbParen (p > appPrec) $
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
    <> showbMaybePrec 0 (modSelector pfs)
    <> ", descrSelector = "
    <> showbMaybePrec 0 (descrSelector pfs)
    <> ", typeSelector = "
    <> showbMaybePrec 0 (typeSelector pfs)
    <> ", ccSelector = "
    <> showbMaybePrec 0 (ccSelector pfs)
    <> ", ccsSelector = "
    <> showbMaybePrec 0 (ccsSelector pfs)
    <> ", retainerSelector = "
    <> showbMaybePrec 0 (retainerSelector pfs)
    <> ", bioSelector = "
    <> showbMaybePrec 0 (bioSelector pfs)
    <> s '}'

-- | Convert a 'TraceFlags' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
-- 
-- /Since: 0.5/
showbTraceFlagsPrec :: Int -> TraceFlags -> Builder
showbTraceFlagsPrec p tfs = showbParen (p > appPrec) $
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
