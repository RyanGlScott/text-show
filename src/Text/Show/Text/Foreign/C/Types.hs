{-# LANGUAGE NoImplicitPrelude #-}
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

import Data.Text.Lazy.Builder (Builder)

import Foreign.C.Types

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()

-- | Convert a 'CChar' to a 'Builder' with the given precedence.
showbCCharPrec :: Int -> CChar -> Builder
showbCCharPrec p (CChar c) = showbPrec p c
{-# INLINE showbCCharPrec #-}

-- | Convert a 'CSChar' to a 'Builder' with the given precedence.
showbCSCharPrec :: Int -> CSChar -> Builder
showbCSCharPrec p (CSChar c) = showbPrec p c
{-# INLINE showbCSCharPrec #-}

-- | Convert a 'CUChar' to a 'Builder'.
showbCUChar :: CUChar -> Builder
showbCUChar (CUChar c) = showb c
{-# INLINE showbCUChar #-}

instance Show CChar where
    showbPrec = showbCCharPrec
    {-# INLINE showbPrec #-}

instance Show CSChar where
    showbPrec = showbCSCharPrec
    {-# INLINE showbPrec #-}

instance Show CUChar where
    showb = showbCUChar
    {-# INLINE showb #-}