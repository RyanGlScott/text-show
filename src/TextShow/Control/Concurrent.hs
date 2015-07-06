{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Concurrent
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for concurrency-related data types.

/Since: 2/
-}
module TextShow.Control.Concurrent (
      showbThreadIdPrec
    , showbThreadStatusPrec
    , showbBlockReason
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Conc (BlockReason, ThreadId, ThreadStatus)

import TextShow.Classes (TextShow(showb, showbPrec), FromStringShow(..))
import TextShow.TH.Internal (deriveTextShow)

#include "inline.h"

-- | Convert a 'ThreadId' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbThreadIdPrec :: Int -> ThreadId -> Builder
showbThreadIdPrec p = showbPrec p . FromStringShow
{-# INLINE showbThreadIdPrec #-}

-- | Convert a 'ThreadStatus' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbThreadStatusPrec :: Int -> ThreadStatus -> Builder
showbThreadStatusPrec = showbPrec
{-# INLINE showbThreadStatusPrec #-}

-- | Convert a 'BlockReason' to a 'Builder'.
--
-- /Since: 2/
showbBlockReason :: BlockReason -> Builder
showbBlockReason = showb
{-# INLINE showbBlockReason #-}

instance TextShow ThreadId where
    showbPrec = showbThreadIdPrec
    INLINE_INST_FUN(showbPrec)

$(deriveTextShow ''ThreadStatus)
$(deriveTextShow ''BlockReason)
