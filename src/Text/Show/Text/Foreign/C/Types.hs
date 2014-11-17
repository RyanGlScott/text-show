{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Foreign.C.Types
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for Haskell newtypes corresponding to C
-- types in the Foreign Function Interface (FFI).
----------------------------------------------------------------------------
module Text.Show.Text.Foreign.C.Types (
      showbCCharPrec
    , showbCSCharPrec
    , showbCUChar
--     , showbCShortPrec
--     , showbCUShort
--     , showbCIntPrec
--     , showbCUInt
--     , showbCLongPrec
--     , showbCULong
--     , showbCPtrdiffPrec
--     , showbCSize
--     , showbCWcharPrec
--     , showbCSigAtomicPrec
--     , showbCLLongPrec
--     , showbCULLong
--     , showbCIntPtrPrec
--     , showbCUIntPtr
--     , showbCIntMaxPrec
--     , showbCUIntMax
--     , showbCClockPrec
--     , showbCTimePrec
--     , showbCUSeconds
--     , showbCSUSecondsPrec
--     , showbCFloatPrec
--     , showbCDoublePrec
    ) where

import Data.Int
import Data.Text.Lazy.Builder (Builder)
import Data.Word

import Foreign.C.Types

import GHC.Prim (unsafeCoerce#)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()

#include "HsBaseConfig.h"

-- | Convert a 'CChar' to a 'Builder' with the given precedence.
showbCCharPrec :: Int -> CChar -> Builder
showbCCharPrec p cchar = showbPrec p (unsafeCoerce# cchar :: HTYPE_CHAR)

-- | Convert a 'CSChar' to a 'Builder' with the given precedence.
showbCSCharPrec :: Int -> CSChar -> Builder
showbCSCharPrec p cschar = showbPrec p (unsafeCoerce# cschar :: HTYPE_SIGNED_CHAR)

-- | Convert a 'CUChar' to a 'Builder'.
showbCUChar :: CUChar -> Builder
showbCUChar cuchar = showb (unsafeCoerce# cuchar :: HTYPE_UNSIGNED_CHAR)

instance Show CChar where
    showbPrec = showbCCharPrec
    {-# INLINE showbPrec #-}

instance Show CSChar where
    showbPrec = showbCSCharPrec
    {-# INLINE showbPrec #-}

instance Show CUChar where
    showb = showbCUChar
    {-# INLINE showb #-}