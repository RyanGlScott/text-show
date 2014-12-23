{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Foreign.C.Types
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for Haskell newtypes corresponding to C
types in the Foreign Function Interface (FFI).
-}
module Text.Show.Text.Foreign.C.Types (
      showbCCharPrec
    , showbCSCharPrec
    , showbCUChar
    , showbCShortPrec
    , showbCUShort
    , showbCIntPrec
    , showbCUInt
    , showbCLongPrec
    , showbCULong
    , showbCPtrdiffPrec
    , showbCSize
    , showbCWcharPrec
    , showbCSigAtomicPrec
    , showbCLLongPrec
    , showbCULLong
    , showbCIntPtrPrec
    , showbCUIntPtr
    , showbCIntMaxPrec
    , showbCUIntMax
    , showbCClockPrec
    , showbCTimePrec
    , showbCUSeconds
    , showbCSUSecondsPrec
    , showbCFloatPrec
    , showbCDoublePrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import Foreign.C.Types

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.Floating ()

-- | Convert a 'CChar' to a 'Builder' with the given precedence.
showbCCharPrec :: Int -> CChar -> Builder
showbCCharPrec = showbPrec
{-# INLINE showbCCharPrec #-}

-- | Convert a 'CSChar' to a 'Builder' with the given precedence.
showbCSCharPrec :: Int -> CSChar -> Builder
showbCSCharPrec = showbPrec
{-# INLINE showbCSCharPrec #-}

-- | Convert a 'CUChar' to a 'Builder'.
showbCUChar :: CUChar -> Builder
showbCUChar = showb
{-# INLINE showbCUChar #-}

-- | Convert a 'CShort' to a 'Builder' with the given precedence.
showbCShortPrec :: Int -> CShort -> Builder
showbCShortPrec = showbPrec
{-# INLINE showbCShortPrec #-}

-- | Convert a 'CUShort' to a 'Builder'.
showbCUShort :: CUShort -> Builder
showbCUShort = showb
{-# INLINE showbCUShort #-}

-- | Convert a 'CInt' to a 'Builder' with the given precedence.
showbCIntPrec :: Int -> CInt -> Builder
showbCIntPrec = showbPrec
{-# INLINE showbCIntPrec #-}

-- | Convert a 'CUInt' to a 'Builder'.
showbCUInt :: CUInt -> Builder
showbCUInt = showb
{-# INLINE showbCUInt #-}

-- | Convert a 'CLong' to a 'Builder' with the given precedence.
showbCLongPrec :: Int -> CLong -> Builder
showbCLongPrec = showbPrec
{-# INLINE showbCLongPrec #-}

-- | Convert a 'CULong' to a 'Builder'.
showbCULong :: CULong -> Builder
showbCULong = showb
{-# INLINE showbCULong #-}

-- | Convert a 'CPtrdiff' to a 'Builder' with the given precedence.
showbCPtrdiffPrec :: Int -> CPtrdiff -> Builder
showbCPtrdiffPrec = showbPrec
{-# INLINE showbCPtrdiffPrec #-}

-- | Convert a 'CSize' to a 'Builder'.
showbCSize :: CSize -> Builder
showbCSize = showb
{-# INLINE showbCSize #-}

-- | Convert a 'CWchar' to a 'Builder' with the given precedence.
showbCWcharPrec :: Int -> CWchar -> Builder
showbCWcharPrec = showbPrec
{-# INLINE showbCWcharPrec #-}

-- | Convert a 'CSigAtomic' to a 'Builder' with the given precedence.
showbCSigAtomicPrec :: Int -> CSigAtomic -> Builder
showbCSigAtomicPrec = showbPrec
{-# INLINE showbCSigAtomicPrec #-}

-- | Convert a 'CLLong' to a 'Builder' with the given precedence.
showbCLLongPrec :: Int -> CLLong -> Builder
showbCLLongPrec = showbPrec
{-# INLINE showbCLLongPrec #-}

-- | Convert a 'CULLong' to a 'Builder'.
showbCULLong :: CULLong -> Builder
showbCULLong = showb
{-# INLINE showbCULLong #-}

-- | Convert a 'CIntPtr' to a 'Builder' with the given precedence.
showbCIntPtrPrec :: Int -> CIntPtr -> Builder
showbCIntPtrPrec = showbPrec
{-# INLINE showbCIntPtrPrec #-}

-- | Convert a 'CUIntPtr' to a 'Builder'.
showbCUIntPtr :: CUIntPtr -> Builder
showbCUIntPtr = showb
{-# INLINE showbCUIntPtr #-}

-- | Convert a 'CIntMax' to a 'Builder' with the given precedence.
showbCIntMaxPrec :: Int -> CIntMax -> Builder
showbCIntMaxPrec = showbPrec
{-# INLINE showbCIntMaxPrec #-}

-- | Convert a 'CUIntMax' to a 'Builder'.
showbCUIntMax :: CUIntMax -> Builder
showbCUIntMax = showb
{-# INLINE showbCUIntMax #-}

-- | Convert a 'CClock' to a 'Builder' with the given precedence.
showbCClockPrec :: Int -> CClock -> Builder
showbCClockPrec = showbPrec
{-# INLINE showbCClockPrec #-}

-- | Convert a 'CTime' to a 'Builder' with the given precedence.
showbCTimePrec :: Int -> CTime -> Builder
showbCTimePrec = showbPrec
{-# INLINE showbCTimePrec #-}

-- | Convert a 'CUSeconds' value to a 'Builder'.
showbCUSeconds :: CUSeconds -> Builder
showbCUSeconds = showb
{-# INLINE showbCUSeconds #-}

-- | Convert a 'CSUSeconds' value to a 'Builder' with the given precedence.
showbCSUSecondsPrec :: Int -> CSUSeconds -> Builder
showbCSUSecondsPrec = showbPrec
{-# INLINE showbCSUSecondsPrec #-}

-- | Convert a 'CFloat' to a 'Builder' with the given precedence.
showbCFloatPrec :: Int -> CFloat -> Builder
showbCFloatPrec = showbPrec
{-# INLINE showbCFloatPrec #-}

-- | Convert a 'CDouble' to a 'Builder' with the given precedence.
showbCDoublePrec :: Int -> CDouble -> Builder
showbCDoublePrec = showbPrec
{-# INLINE showbCDoublePrec #-}

deriving instance Show CChar
deriving instance Show CSChar
deriving instance Show CUChar
deriving instance Show CShort
deriving instance Show CUShort
deriving instance Show CInt
deriving instance Show CUInt
deriving instance Show CLong
deriving instance Show CULong
deriving instance Show CPtrdiff
deriving instance Show CSize
deriving instance Show CWchar
deriving instance Show CSigAtomic
deriving instance Show CLLong
deriving instance Show CULLong
deriving instance Show CIntPtr
deriving instance Show CUIntPtr
deriving instance Show CIntMax
deriving instance Show CUIntMax
deriving instance Show CClock
deriving instance Show CTime
deriving instance Show CUSeconds
deriving instance Show CSUSeconds
deriving instance Show CFloat
deriving instance Show CDouble