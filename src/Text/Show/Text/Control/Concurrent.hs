{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Control.Concurrent
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for concurrency-related data types.
----------------------------------------------------------------------------
module Text.Show.Text.Control.Concurrent (
      showbThreadIdPrec
    , showbThreadStatusPrec
    , showbBlockReason
    ) where

import           Data.Text.Lazy.Builder (Builder, fromString)

import           GHC.Conc (BlockReason(..), ThreadId, ThreadStatus(..))
import           GHC.Show (appPrec)

import qualified Prelude as P
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import           Text.Show.Text.Utils ((<>))

-- | Convert a 'ThreadId' to a 'Builder' with the given precedence.
showbThreadIdPrec :: Int -> ThreadId -> Builder
showbThreadIdPrec p ti = fromString $ P.showsPrec p ti ""
{-# INLINE showbThreadIdPrec #-}

-- | Convert a 'ThreadStatus' to a 'Builder' with the given precedence.
showbThreadStatusPrec :: Int -> ThreadStatus -> Builder
showbThreadStatusPrec _ ThreadRunning  = "ThreadRunning"
showbThreadStatusPrec _ ThreadFinished = "ThreadFinished"
showbThreadStatusPrec p (ThreadBlocked br)
    = showbParen (p > appPrec) $ "ThreadBlocked " <> showbBlockReason br
showbThreadStatusPrec _ ThreadDied     = "ThreadDied"
{-# INLINE showbThreadStatusPrec #-}

-- | Convert a 'BlockReason' to a 'Builder'.
showbBlockReason :: BlockReason -> Builder
showbBlockReason BlockedOnMVar        = "BlockedOnMVar"
showbBlockReason BlockedOnBlackHole   = "BlockedOnBlackHole"
showbBlockReason BlockedOnException   = "BlockedOnException"
showbBlockReason BlockedOnSTM         = "BlockedOnSTM"
showbBlockReason BlockedOnForeignCall = "BlockedOnForeignCall"
showbBlockReason BlockedOnOther       = "BlockedOnOther"
{-# INLINE showbBlockReason #-}

instance Show ThreadId where
    showbPrec = showbThreadIdPrec
    {-# INLINE showbPrec #-}

instance Show ThreadStatus where
    showbPrec = showbThreadStatusPrec
    {-# INLINE showbPrec #-}

instance Show BlockReason where
    showb = showbBlockReason
    {-# INLINE showb #-}