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

'TextShow' instances for concurrency-related data types.

/Since: 2/
-}
module TextShow.Control.Concurrent () where

import Data.Text.Lazy.Builder (fromString)

import Foreign.C.Types

import GHC.Conc (BlockReason, ThreadStatus)
import GHC.Conc.Sync (ThreadId(..))
import GHC.Exts (Addr#, unsafeCoerce#)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Foreign.C.Types ()
import TextShow.TH.Internal (deriveTextShow)

#if MIN_VERSION_base(4,14,0)
import TextShow.Classes (showbParen)
import GHC.Show (appPrec)
#endif

-- | /Since: 2/
instance TextShow ThreadId where
    showbPrec p t =
#if MIN_VERSION_base(4,14,0)
      showbParen (p > appPrec) $
#endif
      fromString "ThreadId " <> showbPrec p (getThreadId t)
    {-# INLINE showbPrec #-}

-- Temporary workaround until Trac #8281 is fixed
foreign import ccall unsafe "rts_getThreadId" getThreadId# :: Addr# -> CInt

getThreadId :: ThreadId -> CInt
getThreadId (ThreadId tid) = getThreadId# (unsafeCoerce# tid)

-- | /Since: 2/
$(deriveTextShow ''BlockReason)
-- | /Since: 2/
$(deriveTextShow ''ThreadStatus)
