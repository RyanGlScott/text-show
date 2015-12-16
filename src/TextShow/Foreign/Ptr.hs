{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Foreign.Ptr
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for pointer types used in the Haskell
Foreign Function Interface (FFI).

/Since: 2/
-}
module TextShow.Foreign.Ptr (
      showbPtr
    , showbFunPtr
    , showbIntPtrPrec
    , showbWordPtr
    , showbForeignPtr
    ) where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, singleton)

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (FunPtr, IntPtr, WordPtr, castFunPtrToPtr)

import GHC.ForeignPtr (unsafeForeignPtrToPtr)
import GHC.Num (wordToInteger)
import GHC.Ptr (Ptr(..))
import GHC.Prim (addr2Int#, int2Word#, unsafeCoerce#)

import TextShow.Classes (TextShow(..), TextShow1(..))
import TextShow.Data.Integral (showbHex, showbIntPrec, showbWord)
import TextShow.Utils (lengthB, mtimesDefault)

#include "MachDeps.h"
#include "inline.h"

-- | Convert a 'Ptr' to a 'Builder'. Note that this does not require the parameterized
-- type to be an instance of 'Show' itself.
--
-- /Since: 2/
showbPtr :: Ptr a -> Builder
showbPtr = showb
{-# INLINE showbPtr #-}

-- | Convert a 'FunPtr' to a 'Builder'. Note that this does not require the
-- parameterized type to be an instance of 'Show' itself.
--
-- /Since: 2/
showbFunPtr :: FunPtr a -> Builder
showbFunPtr = showb
{-# INLINE showbFunPtr #-}

-- | Convert an 'IntPtr' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntPtrPrec :: Int -> IntPtr -> Builder
showbIntPtrPrec p ip = showbIntPrec p $ unsafeCoerce# ip

-- | Convert a 'WordPtr' to a 'Builder'.
--
-- /Since: 2/
showbWordPtr :: WordPtr -> Builder
showbWordPtr wp = showbWord $ unsafeCoerce# wp

-- | Convert a 'ForeignPtr' to a 'Builder'. Note that this does not require the
-- parameterized type to be an instance of 'Show' itself.
--
-- /Since: 2/
showbForeignPtr :: ForeignPtr a -> Builder
showbForeignPtr = showb
{-# INLINE showbForeignPtr #-}

instance TextShow (Ptr a) where
    showbPrec = showbPrecWith undefined
    INLINE_INST_FUN(showbPrec)

instance TextShow1 Ptr where
    showbPrecWith _ _ (Ptr a) = padOut . showbHex $ wordToInteger (int2Word# (addr2Int# a))
      where
        padOut :: Builder -> Builder
        padOut ls =
             singleton '0' <> singleton 'x'
          <> mtimesDefault (max 0 $ 2*SIZEOF_HSPTR - lengthB ls) (singleton '0')
          <> ls

instance TextShow (FunPtr a) where
    showbPrec = showbPrecWith undefined
    INLINE_INST_FUN(showbPrec)

instance TextShow1 FunPtr where
    showbPrecWith _ _ = showb . castFunPtrToPtr
    INLINE_INST_FUN(showbPrecWith)

instance TextShow IntPtr where
    showbPrec = showbIntPtrPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow WordPtr where
    showb = showbWordPtr
    INLINE_INST_FUN(showb)

instance TextShow (ForeignPtr a) where
    showbPrec = showbPrecWith undefined
    INLINE_INST_FUN(showbPrec)

instance TextShow1 ForeignPtr where
    showbPrecWith _ _ = showb . unsafeForeignPtrToPtr
    INLINE_INST_FUN(showbPrecWith)
