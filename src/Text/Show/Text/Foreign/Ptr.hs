{-# LANGUAGE CPP, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Foreign.Ptr
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for pointer types used in the Haskell
Foreign Function Interface (FFI).

/Since: 0.3/
-}
module Text.Show.Text.Foreign.Ptr (
      showbPtr
    , showbFunPtr
    , showbIntPtrPrec
    , showbWordPtr
    , showbForeignPtr
    ) where

import Data.Semigroup (timesN)
import Data.Text.Lazy.Builder (Builder)

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (FunPtr, IntPtr, WordPtr, castFunPtrToPtr)

import GHC.ForeignPtr (unsafeForeignPtrToPtr)
import GHC.Num (wordToInteger)
import GHC.Ptr (Ptr(..))
import GHC.Prim (addr2Int#, int2Word#, unsafeCoerce#)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.Data.Integral (showbHex, showbIntPrec, showbWord)
import Text.Show.Text.Utils ((<>), lengthB, s)

#include "MachDeps.h"
#include "inline.h"

-- | Convert a 'Ptr' to a 'Builder'. Note that this does not require the parameterized
-- type to be an instance of 'Show' itself.
-- 
-- /Since: 0.3/
showbPtr :: Ptr a -> Builder
showbPtr (Ptr a) = padOut . showbHex $ wordToInteger (int2Word# (addr2Int# a))
  where
    padOut :: Builder -> Builder
    padOut ls =
         s '0' <> s 'x'
      <> timesN (fromIntegral . max 0 $ 2*SIZEOF_HSPTR - lengthB ls) (s '0')
      <> ls

-- | Convert a 'FunPtr' to a 'Builder'. Note that this does not require the
-- parameterized type to be an instance of 'Show' itself.
-- 
-- /Since: 0.3/
showbFunPtr :: FunPtr a -> Builder
showbFunPtr = showb . castFunPtrToPtr
{-# INLINE showbFunPtr #-}

-- | Convert an 'IntPtr' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbIntPtrPrec :: Int -> IntPtr -> Builder
showbIntPtrPrec p ip = showbIntPrec p $ unsafeCoerce# ip

-- | Convert a 'WordPtr' to a 'Builder'.
-- 
-- /Since: 0.3/
showbWordPtr :: WordPtr -> Builder
showbWordPtr wp = showbWord $ unsafeCoerce# wp

-- | Convert a 'ForeignPtr' to a 'Builder'. Note that this does not require the
-- parameterized type to be an instance of 'Show' itself.
-- 
-- /Since: 0.3/
showbForeignPtr :: ForeignPtr a -> Builder
showbForeignPtr = showb . unsafeForeignPtrToPtr
{-# INLINE showbForeignPtr #-}

instance Show (Ptr a) where
    showb = showbPtr
    INLINE_INST_FUN(showb)

instance Show1 Ptr where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show (FunPtr a) where
    showb = showbFunPtr
    INLINE_INST_FUN(showb)

instance Show1 FunPtr where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show IntPtr where
    showbPrec = showbIntPtrPrec
    INLINE_INST_FUN(showbPrec)

instance Show WordPtr where
    showb = showbWordPtr
    INLINE_INST_FUN(showb)

instance Show (ForeignPtr a) where
    showb = showbForeignPtr
    INLINE_INST_FUN(showb)

instance Show1 ForeignPtr where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)