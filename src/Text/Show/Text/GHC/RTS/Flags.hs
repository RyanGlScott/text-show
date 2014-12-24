{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.RTS.Flags
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the 'GHC.RTS.Flags' module.
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

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec))
import Text.Show.Text.Data.Bool     ()
import Text.Show.Text.Data.Char     ()
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List     ()
import Text.Show.Text.Data.Maybe    ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert an 'RTSFlags' value to a 'Builder' with the given precedence.
showbRTSFlagsPrec :: Int -> RTSFlags -> Builder
showbRTSFlagsPrec = showbPrec
{-# INLINE showbRTSFlagsPrec #-}

-- | Convert a 'GCFlags' value to a 'Builder' with the given precedence.
showbGCFlagsPrec :: Int -> GCFlags -> Builder
showbGCFlagsPrec = undefined
{-# INLINE showbGCFlagsPrec #-}

-- | Convert a 'ConcFlags' value to a 'Builder' with the given precedence.
showbConcFlagsPrec :: Int -> ConcFlags -> Builder
showbConcFlagsPrec = showbPrec
{-# INLINE showbConcFlagsPrec #-}

-- | Convert a 'MiscFlags' value to a 'Builder' with the given precedence.
showbMiscFlagsPrec :: Int -> MiscFlags -> Builder
showbMiscFlagsPrec = showbPrec
{-# INLINE showbMiscFlagsPrec #-}

-- | Convert a 'DebugFlags' value to a 'Builder' with the given precedence.
showbDebugFlagsPrec :: Int -> DebugFlags -> Builder
showbDebugFlagsPrec = showbPrec
{-# INLINE showbDebugFlagsPrec #-}

-- | Convert a 'CCFlags' value to a 'Builder' with the given precedence.
showbCCFlagsPrec :: Int -> CCFlags -> Builder
showbCCFlagsPrec = undefined
{-# INLINE showbCCFlagsPrec #-}

-- | Convert a 'ProfFlags' value to a 'Builder' with the given precedence.
showbProfFlagsPrec :: Int -> ProfFlags -> Builder
showbProfFlagsPrec = undefined
{-# INLINE showbProfFlagsPrec #-}

-- | Convert a 'TraceFlags' value to a 'Builder' with the given precedence.
showbTraceFlagsPrec :: Int -> TraceFlags -> Builder
showbTraceFlagsPrec = undefined
{-# INLINE showbTraceFlagsPrec #-}

-- | Convert a 'TickyFlags' value to a 'Builder' with the given precedence.
showbTickyFlagsPrec :: Int -> TickyFlags -> Builder
showbTickyFlagsPrec = showbPrec
{-# INLINE showbTickyFlagsPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''RTSFlags)

instance Show GCFlags where
    showbPrec = showbGCFlagsPrec
    {-# INLINE showbPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''ConcFlags)

$(deriveShowPragmas defaultInlineShowbPrec ''MiscFlags)

$(deriveShowPragmas defaultInlineShowbPrec ''DebugFlags)

instance Show CCFlags where
    showbPrec = showbCCFlagsPrec
    {-# INLINE showbPrec #-}

instance Show ProfFlags where
    showbPrec = showbProfFlagsPrec
    {-# INLINE showbPrec #-}

instance Show TraceFlags where
    showbPrec = showbTraceFlagsPrec
    {-# INLINE showbPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''TickyFlags)
