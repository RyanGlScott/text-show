{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.GHC.Stats
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'GCStats'.
----------------------------------------------------------------------------
module Text.Show.Text.GHC.Stats (showbGCStatsPrec) where 

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec)
import GHC.Stats (GCStats(..))

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec), showbParen)
import Text.Show.Text.Data.Integral (showbInt64Prec)
import Text.Show.Text.Data.Floating (showbDoublePrec)
import Text.Show.Text.Utils (s)

-- | Convert a 'GCStats' value to a 'Builder' with the given precedence.
showbGCStatsPrec :: Int -> GCStats -> Builder
showbGCStatsPrec p gcStats = showbParen (p > appPrec) $
       "GCStats {bytesAllocated = "
    <> showbInt64Prec 0 (bytesAllocated gcStats)
    <> ", numGcs = "
    <> showbInt64Prec 0 (numGcs gcStats)
    <> ", maxBytesUsed = "
    <> showbInt64Prec 0 (maxBytesUsed gcStats)
    <> ", numByteUsageSamples = "
    <> showbInt64Prec 0 (numByteUsageSamples gcStats)
    <> ", cumulativeBytesUsed = "
    <> showbInt64Prec 0 (cumulativeBytesUsed gcStats)
    <> ", bytesCopied = "
    <> showbInt64Prec 0 (bytesCopied gcStats)
    <> ", currentBytesUsed = "
    <> showbInt64Prec 0 (currentBytesUsed gcStats)
    <> ", currentBytesSlop = "
    <> showbInt64Prec 0 (currentBytesSlop gcStats)
    <> ", maxBytesSlop = "
    <> showbInt64Prec 0 (maxBytesSlop gcStats)
    <> ", peakMegabytesAllocated = "
    <> showbInt64Prec 0 (peakMegabytesAllocated gcStats)
    <> ", mutatorCpuSeconds = "
    <> showbDoublePrec 0 (mutatorCpuSeconds gcStats)
    <> ", mutatorWallSeconds = "
    <> showbDoublePrec 0 (mutatorWallSeconds gcStats)
    <> ", gcCpuSeconds = "
    <> showbDoublePrec 0 (gcCpuSeconds gcStats)
    <> ", gcWallSeconds = "
    <> showbDoublePrec 0 (gcWallSeconds gcStats)
    <> ", cpuSeconds = "
    <> showbDoublePrec 0 (cpuSeconds gcStats)
    <> ", wallSeconds = "
    <> showbDoublePrec 0 (wallSeconds gcStats)
#if MIN_VERSION_base(4,6,0)
    <> ", parTotBytesCopied = "
    <> showbInt64Prec 0 (parTotBytesCopied gcStats)
#else
    <> ", parAvgBytesCopied = "
    <> showbInt64Prec 0 (parAvgBytesCopied gcStats)
#endif
    <> ", parMaxBytesCopied = "
    <> showbInt64Prec 0 (parMaxBytesCopied gcStats)
    <> s '}'
{-# INLINE showbGCStatsPrec #-}

instance Show GCStats where
    showbPrec = showbGCStatsPrec
    {-# INLINE showbPrec #-}