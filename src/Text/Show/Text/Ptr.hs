{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Ptr where

import Data.Monoid ((<>))
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)
import Data.Word (Word)

import Foreign.Ptr (FunPtr, IntPtr, WordPtr, castFunPtrToPtr)

import GHC.Num (wordToInteger)
import GHC.Ptr (Ptr(..))
import GHC.Prim (addr2Int#, int2Word#, unsafeCoerce#)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(..))
import Text.Show.Text.Functions (lengthB, replicateB, s)
import Text.Show.Text.Int (showbHex, showbInt)

#include "MachDeps.h"

showbPtr :: Ptr a -> Builder
showbPtr (Ptr a) = padOut . showbHex $ wordToInteger (int2Word# (addr2Int# a))
  where
    padOut :: Builder -> Builder
    padOut ls =    s '0' <> s 'x'
                <> replicateB (2*SIZEOF_HSPTR - lengthB ls) (s '0')
                <> ls

showbIntPtr :: Int -> IntPtr -> Builder
showbIntPtr k ip = showbInt k (unsafeCoerce# ip :: Int)

showbWordPtr :: WordPtr -> Builder
showbWordPtr wp = build (unsafeCoerce# wp :: Word)

instance Show (Ptr a) where
    showb = showbPtr
    {-# INLINE showb #-}

instance Show (FunPtr a) where
    showbPrec p = showbPrec p . castFunPtrToPtr
    {-# INLINE showbPrec #-}

instance Show IntPtr where
    showbPrec = showbIntPtr
    {-# INLINE showbPrec #-}

instance Show WordPtr where
    showb = showbWordPtr
    {-# INLINE showb #-}