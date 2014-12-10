{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Concurrent
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for concurrency-related data types.
-}
module Text.Show.Text.Control.Concurrent (
      showbThreadIdPrec
    , showbThreadStatusPrec
    , showbBlockReason
    ) where

import           Data.Text.Lazy.Builder (Builder, fromString)

import           GHC.Conc (BlockReason, ThreadId, ThreadStatus)

import qualified Prelude as P
import           Prelude hiding (Show)

import           Text.Show.Text.Classes (Show(showb, showbPrec))
import           Text.Show.Text.TH.Internal (deriveShow)

-- | Convert a 'ThreadId' to a 'Builder' with the given precedence.
showbThreadIdPrec :: Int -> ThreadId -> Builder
showbThreadIdPrec p ti = fromString $ P.showsPrec p ti ""
{-# INLINE showbThreadIdPrec #-}

-- | Convert a 'ThreadStatus' to a 'Builder' with the given precedence.
showbThreadStatusPrec :: Int -> ThreadStatus -> Builder
showbThreadStatusPrec = showbPrec
{-# INLINE showbThreadStatusPrec #-}

-- | Convert a 'BlockReason' to a 'Builder'.
showbBlockReason :: BlockReason -> Builder
showbBlockReason = showb
{-# INLINE showbBlockReason #-}

instance Show ThreadId where
    showbPrec = showbThreadIdPrec
    {-# INLINE showbPrec #-}

$(deriveShow ''ThreadStatus)
$(deriveShow ''BlockReason)