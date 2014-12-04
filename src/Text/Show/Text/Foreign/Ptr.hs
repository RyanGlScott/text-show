{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Foreign.Ptr
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for pointer types used in the Haskell
-- Foreign Function Interface (FFI).
----------------------------------------------------------------------------
module Text.Show.Text.Foreign.Ptr (
      showbPtr
    , showbFunPtr
    , showbIntPtrPrec
    , showbWordPtr
    , showbForeignPtr
    ) where

import Data.Text.Lazy.Builder (Builder)

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (FunPtr, IntPtr, WordPtr, castFunPtrToPtr)

import GHC.Num (wordToInteger)
import GHC.Ptr (Ptr(..))
import GHC.Prim (addr2Int#, int2Word#, unsafeCoerce#)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral (showbHex, showbIntPrec, showbWord)
import Text.Show.Text.Utils ((<>), lengthB, replicateB, s)

#include "MachDeps.h"

-- | Convert a 'Ptr' to a 'Builder'. Note that this does not require the parameterized
--   type to be an instance of 'Show' itself.
showbPtr :: Ptr a -> Builder
showbPtr (Ptr a) = padOut . showbHex $ wordToInteger (int2Word# (addr2Int# a))
  where
    padOut :: Builder -> Builder
    padOut ls =    s '0' <> s 'x'
                <> replicateB (2*SIZEOF_HSPTR - lengthB ls) (s '0')
                <> ls

-- | Convert a 'FunPtr' to a 'Builder'. Note that this does not require the
--   parameterized type to be an instance of 'Show' itself.
showbFunPtr :: FunPtr a -> Builder
showbFunPtr = showb . castFunPtrToPtr
{-# INLINE showbFunPtr #-}

-- | Convert an 'IntPtr' to a 'Builder' with the given precedence.
showbIntPtrPrec :: Int -> IntPtr -> Builder
showbIntPtrPrec k ip = showbIntPrec k $ unsafeCoerce# ip

-- | Convert a 'WordPtr' to a 'Builder'.
showbWordPtr :: WordPtr -> Builder
showbWordPtr wp = showbWord $ unsafeCoerce# wp

-- | Convert a 'ForeignPtr' to a 'Builder'. Note that this does not require the
--   parameterized type to be an instance of 'Show' itself.
showbForeignPtr :: ForeignPtr a -> Builder
showbForeignPtr = showb . unsafeForeignPtrToPtr
{-# INLINE showbForeignPtr #-}

instance Show (Ptr a) where
    showb = showbPtr
    {-# INLINE showb #-}

instance Show (FunPtr a) where
    showb = showbFunPtr
    {-# INLINE showb #-}

instance Show IntPtr where
    showbPrec = showbIntPtrPrec
    {-# INLINE showbPrec #-}

instance Show WordPtr where
    showb = showbWordPtr
    {-# INLINE showb #-}

instance Show (ForeignPtr a) where
    showb = showbForeignPtr
    {-# INLINE showb #-}