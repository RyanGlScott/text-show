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

'TextShow' instances for concurrency-related data types.

/Since: 2/
-}
module TextShow.Control.Concurrent () where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (fromString)

import Foreign.C.Types

import GHC.Conc (BlockReason, ThreadStatus)
import GHC.Conc.Sync (ThreadId(..))
import GHC.Prim

import TextShow.Classes (TextShow(..))
import TextShow.Foreign.C.Types ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
instance TextShow ThreadId where
    showbPrec p t = fromString "ThreadId " <> showbPrec p (getThreadId t)
    {-# INLINE showbPrec #-}

-- Temporary workaround until Trac #8281 is fixed
foreign import ccall unsafe "rts_getThreadId" getThreadId# :: Addr# -> CInt

getThreadId :: ThreadId -> CInt
getThreadId (ThreadId tid) = getThreadId# (unsafeCoerce# tid)

-- | /Since: 2/
$(deriveTextShow ''ThreadStatus)
-- | /Since: 2/
$(deriveTextShow ''BlockReason)
