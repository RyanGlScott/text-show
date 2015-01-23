{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Concurrent
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for concurrency-related data types.

/Since: 0.3/
-}
module Text.Show.Text.Control.Concurrent (
      showbThreadIdPrec
    , showbThreadStatusPrec
    , showbBlockReason
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Conc (BlockReason, ThreadId, ThreadStatus)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), FromStringShow(..))
import Text.Show.Text.TH.Internal (deriveShow)

#include "inline.h"

-- | Convert a 'ThreadId' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbThreadIdPrec :: Int -> ThreadId -> Builder
showbThreadIdPrec p = showbPrec p . FromStringShow
{-# INLINE showbThreadIdPrec #-}

-- | Convert a 'ThreadStatus' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbThreadStatusPrec :: Int -> ThreadStatus -> Builder
showbThreadStatusPrec = showbPrec
{-# INLINE showbThreadStatusPrec #-}

-- | Convert a 'BlockReason' to a 'Builder'.
-- 
-- /Since: 0.3/
showbBlockReason :: BlockReason -> Builder
showbBlockReason = showb
{-# INLINE showbBlockReason #-}

instance Show ThreadId where
    showbPrec = showbThreadIdPrec
    INLINE_INST_FUN(showbPrec)

$(deriveShow ''ThreadStatus)
$(deriveShow ''BlockReason)