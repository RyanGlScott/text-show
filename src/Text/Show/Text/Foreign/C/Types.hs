{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
#else
{-# LANGUAGE MagicHash #-}
#endif
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
#if MIN_VERSION_base(4,4,0)
    , showbCUSeconds
    , showbCSUSecondsPrec
#endif
    , showbCFloatPrec
    , showbCDoublePrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import Foreign.C.Types

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec))
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.Data.Integral ()

#if !(MIN_VERSION_base(4,5,0))
import GHC.Prim (unsafeCoerce#)

import Text.Show.Text.Data.Floating (showbDoublePrec, showbFloatPrec)
import Text.Show.Text.Data.Integral ( showbInt8Prec
                                    , showbInt16Prec
                                    , showbInt32Prec
                                    , showbInt64Prec
                                    , showbWord8
                                    , showbWord16
                                    , showbWord32
                                    , showbWord64
                                    )

# include "inline.h"
#endif

-- | Convert a 'CChar' to a 'Builder' with the given precedence.
showbCCharPrec :: Int -> CChar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCCharPrec = showbPrec
{-# INLINE showbCCharPrec #-}
#else
showbCCharPrec p c = showbInt8Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CSChar' to a 'Builder' with the given precedence.
showbCSCharPrec :: Int -> CSChar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSCharPrec = showbPrec
{-# INLINE showbCSCharPrec #-}
#else
showbCSCharPrec p c = showbInt8Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CUChar' to a 'Builder'.
showbCUChar :: CUChar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUChar = showb
{-# INLINE showbCUChar #-}
#else
showbCUChar c = showbWord8 $ unsafeCoerce# c
#endif

-- | Convert a 'CShort' to a 'Builder' with the given precedence.
showbCShortPrec :: Int -> CShort -> Builder
#if MIN_VERSION_base(4,5,0)
showbCShortPrec = showbPrec
{-# INLINE showbCShortPrec #-}
#else
showbCShortPrec p c = showbInt16Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CUShort' to a 'Builder'.
showbCUShort :: CUShort -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUShort = showb
{-# INLINE showbCUShort #-}
#else
showbCUShort c = showbWord16 $ unsafeCoerce# c
#endif

-- | Convert a 'CInt' to a 'Builder' with the given precedence.
showbCIntPrec :: Int -> CInt -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIntPrec = showbPrec
{-# INLINE showbCIntPrec #-}
#else
showbCIntPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CUInt' to a 'Builder'.
showbCUInt :: CUInt -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUInt = showb
{-# INLINE showbCUInt #-}
#else
showbCUInt c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CLong' to a 'Builder' with the given precedence.
showbCLongPrec :: Int -> CLong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCLongPrec = showbPrec
{-# INLINE showbCLongPrec #-}
#else
showbCLongPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CULong' to a 'Builder'.
showbCULong :: CULong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCULong = showb
{-# INLINE showbCULong #-}
#else
showbCULong c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CPtrdiff' to a 'Builder' with the given precedence.
showbCPtrdiffPrec :: Int -> CPtrdiff -> Builder
#if MIN_VERSION_base(4,5,0)
showbCPtrdiffPrec = showbPrec
{-# INLINE showbCPtrdiffPrec #-}
#else
showbCPtrdiffPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CSize' to a 'Builder'.
showbCSize :: CSize -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSize = showb
{-# INLINE showbCSize #-}
#else
showbCSize c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CWchar' to a 'Builder' with the given precedence.
showbCWcharPrec :: Int -> CWchar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCWcharPrec = showbPrec
{-# INLINE showbCWcharPrec #-}
#else
showbCWcharPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CSigAtomic' to a 'Builder' with the given precedence.
showbCSigAtomicPrec :: Int -> CSigAtomic -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSigAtomicPrec = showbPrec
{-# INLINE showbCSigAtomicPrec #-}
#else
showbCSigAtomicPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CLLong' to a 'Builder' with the given precedence.
showbCLLongPrec :: Int -> CLLong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCLLongPrec = showbPrec
{-# INLINE showbCLLongPrec #-}
#else
showbCLLongPrec p c = showbInt64Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CULLong' to a 'Builder'.
showbCULLong :: CULLong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCULLong = showb
{-# INLINE showbCULLong #-}
#else
showbCULLong c = showbWord64 $ unsafeCoerce# c
#endif

-- | Convert a 'CIntPtr' to a 'Builder' with the given precedence.
showbCIntPtrPrec :: Int -> CIntPtr -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIntPtrPrec = showbPrec
{-# INLINE showbCIntPtrPrec #-}
#else
showbCIntPtrPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CUIntPtr' to a 'Builder'.
showbCUIntPtr :: CUIntPtr -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUIntPtr = showb
{-# INLINE showbCUIntPtr #-}
#else
showbCUIntPtr c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CIntMax' to a 'Builder' with the given precedence.
showbCIntMaxPrec :: Int -> CIntMax -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIntMaxPrec = showbPrec
{-# INLINE showbCIntMaxPrec #-}
#else
showbCIntMaxPrec p c = showbInt64Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CUIntMax' to a 'Builder'.
showbCUIntMax :: CUIntMax -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUIntMax = showb
{-# INLINE showbCUIntMax #-}
#else
showbCUIntMax c = showbWord64 $ unsafeCoerce# c
#endif

-- | Convert a 'CClock' to a 'Builder' with the given precedence.
showbCClockPrec :: Int -> CClock -> Builder
#if MIN_VERSION_base(4,5,0)
showbCClockPrec = showbPrec
{-# INLINE showbCClockPrec #-}
#else
showbCClockPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CTime' to a 'Builder' with the given precedence.
showbCTimePrec :: Int -> CTime -> Builder
#if MIN_VERSION_base(4,5,0)
showbCTimePrec = showbPrec
{-# INLINE showbCTimePrec #-}
#else
showbCTimePrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

#if MIN_VERSION_base(4,4,0)
-- | Convert a 'CUSeconds' value to a 'Builder'.
showbCUSeconds :: CUSeconds -> Builder
# if MIN_VERSION_base(4,5,0)
showbCUSeconds = showb
{-# INLINE showbCUSeconds #-}
# else
showbCUSeconds c = showbWord32 $ unsafeCoerce# c
# endif

-- | Convert a 'CSUSeconds' value to a 'Builder' with the given precedence.
showbCSUSecondsPrec :: Int -> CSUSeconds -> Builder
# if MIN_VERSION_base(4,5,0)
showbCSUSecondsPrec = showbPrec
{-# INLINE showbCSUSecondsPrec #-}
# else
showbCSUSecondsPrec p c = showbInt32Prec p $ unsafeCoerce# c
# endif
#endif

-- | Convert a 'CFloat' to a 'Builder' with the given precedence.
showbCFloatPrec :: Int -> CFloat -> Builder
#if MIN_VERSION_base(4,5,0)
showbCFloatPrec = showbPrec
{-# INLINE showbCFloatPrec #-}
#else
showbCFloatPrec p c = showbFloatPrec p $ unsafeCoerce# c
#endif

-- | Convert a 'CDouble' to a 'Builder' with the given precedence.
showbCDoublePrec :: Int -> CDouble -> Builder
#if MIN_VERSION_base(4,5,0)
showbCDoublePrec = showbPrec
{-# INLINE showbCDoublePrec #-}
#else
showbCDoublePrec p c = showbDoublePrec p $ unsafeCoerce# c
#endif

#if MIN_VERSION_base(4,5,0)
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
# if MIN_VERSION_base(4,4,0)
deriving instance Show CUSeconds
deriving instance Show CSUSeconds
# endif
deriving instance Show CFloat
deriving instance Show CDouble
#else
instance Show CChar where
    showbPrec = showbCCharPrec
    INLINE(showbPrec)

instance Show CSChar where
    showbPrec = showbCSCharPrec
    INLINE(showbPrec)

instance Show CUChar where
    showb = showbCUChar
    INLINE(showb)

instance Show CShort where
    showbPrec = showbCShortPrec
    INLINE(showbPrec)

instance Show CUShort where
    showb = showbCUShort
    INLINE(showb)

instance Show CInt where
    showbPrec = showbCIntPrec
    INLINE(showbPrec)

instance Show CUInt where
    showb = showbCUInt
    INLINE(showb)

instance Show CLong where
    showbPrec = showbCLongPrec
    INLINE(showbPrec)

instance Show CULong where
    showb = showbCULong
    INLINE(showb)

instance Show CPtrdiff where
    showbPrec = showbCPtrdiffPrec
    INLINE(showbPrec)

instance Show CSize where
    showb = showbCSize
    INLINE(showb)

instance Show CWchar where
    showbPrec = showbCWcharPrec
    INLINE(showbPrec)

instance Show CSigAtomic where
    showbPrec = showbCSigAtomicPrec
    INLINE(showbPrec)

instance Show CLLong where
    showbPrec = showbCLLongPrec
    INLINE(showbPrec)

instance Show CULLong where
    showb = showbCULLong
    INLINE(showb)

instance Show CIntPtr where
    showbPrec = showbCIntPtrPrec
    INLINE(showbPrec)

instance Show CUIntPtr where
    showb = showbCUIntPtr
    INLINE(showb)

instance Show CIntMax where
    showbPrec = showbCIntMaxPrec
    INLINE(showbPrec)

instance Show CUIntMax where
    showb = showbCUIntMax
    INLINE(showb)

instance Show CClock where
    showbPrec = showbCClockPrec
    INLINE(showbPrec)

instance Show CTime where
    showbPrec = showbCTimePrec
    INLINE(showbPrec)

# if MIN_VERSION_base(4,4,0)
instance Show CUSeconds where
    showb = showbCUSeconds
    INLINE(showb)

instance Show CSUSeconds where
    showbPrec = showbCSUSecondsPrec
    INLINE(showbPrec)
# endif

instance Show CFloat where
    showbPrec = showbCFloatPrec
    INLINE(showbPrec)

instance Show CDouble where
    showbPrec = showbDoublePrec
    INLINE(showbPrec)
#endif