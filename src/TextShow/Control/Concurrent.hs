{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Concurrent
Copyright:   (C) 2014-2017 Ryan Scott
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

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString)

import Foreign.C.Types

import GHC.Conc (BlockReason, ThreadStatus)
import GHC.Conc.Sync (ThreadId(..))
import GHC.Prim

import TextShow.Classes (TextShow(..))
import TextShow.Foreign.C.Types (showbCIntPrec)
import TextShow.TH.Internal (deriveTextShow)

#include "inline.h"

-- | Convert a 'ThreadId' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbThreadIdPrec :: Int -> ThreadId -> Builder
showbThreadIdPrec p t = fromString "ThreadId " <> showbCIntPrec p (getThreadId t)
{-# INLINE showbThreadIdPrec #-}

-- Temporary workaround until Trac #8281 is fixed
foreign import ccall unsafe "rts_getThreadId" getThreadId# :: Addr# -> CInt

getThreadId :: ThreadId -> CInt
getThreadId (ThreadId tid) = getThreadId# (unsafeCoerce# tid)

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
