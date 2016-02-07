{-# LANGUAGE CPP                        #-}
#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Foreign.C.Types
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for Haskell newtypes corresponding to C
types in the Foreign Function Interface (FFI).

/Since: 2/
-}
module TextShow.Foreign.C.Types (
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

import TextShow.Classes (TextShow(..))
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()

#if !(MIN_VERSION_base(4,5,0))
import Data.Int
import Data.Word

import Unsafe.Coerce (unsafeCoerce)

# include "HsBaseConfig.h"
# include "inline.h"
#endif

-- | Convert a 'CChar' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCCharPrec :: Int -> CChar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCCharPrec = showbPrec
{-# INLINE showbCCharPrec #-}
#else
showbCCharPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_CHAR -> Builder)
#endif

-- | Convert a 'CSChar' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCSCharPrec :: Int -> CSChar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSCharPrec = showbPrec
{-# INLINE showbCSCharPrec #-}
#else
showbCSCharPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SIGNED_CHAR -> Builder)
#endif

-- | Convert a 'CUChar' to a 'Builder'.
--
-- /Since: 2/
showbCUChar :: CUChar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUChar = showb
{-# INLINE showbCUChar #-}
#else
showbCUChar = unsafeCoerce (showb :: HTYPE_UNSIGNED_CHAR -> Builder)
#endif

-- | Convert a 'CShort' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCShortPrec :: Int -> CShort -> Builder
#if MIN_VERSION_base(4,5,0)
showbCShortPrec = showbPrec
{-# INLINE showbCShortPrec #-}
#else
showbCShortPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SHORT -> Builder)
#endif

-- | Convert a 'CUShort' to a 'Builder'.
--
-- /Since: 2/
showbCUShort :: CUShort -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUShort = showb
{-# INLINE showbCUShort #-}
#else
showbCUShort = unsafeCoerce (showb :: HTYPE_UNSIGNED_SHORT -> Builder)
#endif

-- | Convert a 'CInt' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCIntPrec :: Int -> CInt -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIntPrec = showbPrec
{-# INLINE showbCIntPrec #-}
#else
showbCIntPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_INT -> Builder)
#endif

-- | Convert a 'CUInt' to a 'Builder'.
--
-- /Since: 2/
showbCUInt :: CUInt -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUInt = showb
{-# INLINE showbCUInt #-}
#else
showbCUInt = unsafeCoerce (showb :: HTYPE_UNSIGNED_INT -> Builder)
#endif

-- | Convert a 'CLong' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCLongPrec :: Int -> CLong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCLongPrec = showbPrec
{-# INLINE showbCLongPrec #-}
#else
showbCLongPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_LONG -> Builder)
#endif

-- | Convert a 'CULong' to a 'Builder'.
--
-- /Since: 2/
showbCULong :: CULong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCULong = showb
{-# INLINE showbCULong #-}
#else
showbCULong = unsafeCoerce (showb :: HTYPE_UNSIGNED_LONG -> Builder)
#endif

-- | Convert a 'CPtrdiff' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCPtrdiffPrec :: Int -> CPtrdiff -> Builder
#if MIN_VERSION_base(4,5,0)
showbCPtrdiffPrec = showbPrec
{-# INLINE showbCPtrdiffPrec #-}
#else
showbCPtrdiffPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_PTRDIFF_T -> Builder)
#endif

-- | Convert a 'CSize' to a 'Builder'.
--
-- /Since: 2/
showbCSize :: CSize -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSize = showb
{-# INLINE showbCSize #-}
#else
showbCSize = unsafeCoerce (showb :: HTYPE_SIZE_T -> Builder)
#endif

-- | Convert a 'CWchar' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCWcharPrec :: Int -> CWchar -> Builder
#if MIN_VERSION_base(4,5,0)
showbCWcharPrec = showbPrec
{-# INLINE showbCWcharPrec #-}
#else
showbCWcharPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_WCHAR_T -> Builder)
#endif

-- | Convert a 'CSigAtomic' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCSigAtomicPrec :: Int -> CSigAtomic -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSigAtomicPrec = showbPrec
{-# INLINE showbCSigAtomicPrec #-}
#else
showbCSigAtomicPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SIG_ATOMIC_T -> Builder)
#endif

-- | Convert a 'CLLong' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCLLongPrec :: Int -> CLLong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCLLongPrec = showbPrec
{-# INLINE showbCLLongPrec #-}
#else
showbCLLongPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_LONG_LONG -> Builder)
#endif

-- | Convert a 'CULLong' to a 'Builder'.
--
-- /Since: 2/
showbCULLong :: CULLong -> Builder
#if MIN_VERSION_base(4,5,0)
showbCULLong = showb
{-# INLINE showbCULLong #-}
#else
showbCULLong = unsafeCoerce (showb :: HTYPE_UNSIGNED_LONG_LONG -> Builder)
#endif

-- | Convert a 'CIntPtr' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCIntPtrPrec :: Int -> CIntPtr -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIntPtrPrec = showbPrec
{-# INLINE showbCIntPtrPrec #-}
#else
showbCIntPtrPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_INTPTR_T -> Builder)
#endif

-- | Convert a 'CUIntPtr' to a 'Builder'.
--
-- /Since: 2/
showbCUIntPtr :: CUIntPtr -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUIntPtr = showb
{-# INLINE showbCUIntPtr #-}
#else
showbCUIntPtr = unsafeCoerce (showb :: HTYPE_UINTPTR_T -> Builder)
#endif

-- | Convert a 'CIntMax' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCIntMaxPrec :: Int -> CIntMax -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIntMaxPrec = showbPrec
{-# INLINE showbCIntMaxPrec #-}
#else
showbCIntMaxPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_INTMAX_T -> Builder)
#endif

-- | Convert a 'CUIntMax' to a 'Builder'.
--
-- /Since: 2/
showbCUIntMax :: CUIntMax -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUIntMax = showb
{-# INLINE showbCUIntMax #-}
#else
showbCUIntMax = unsafeCoerce (showb :: HTYPE_UINTMAX_T -> Builder)
#endif

-- | Convert a 'CClock' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCClockPrec :: Int -> CClock -> Builder
#if MIN_VERSION_base(4,5,0)
showbCClockPrec = showbPrec
{-# INLINE showbCClockPrec #-}
#else
showbCClockPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_CLOCK_T -> Builder)
#endif

-- | Convert a 'CTime' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCTimePrec :: Int -> CTime -> Builder
#if MIN_VERSION_base(4,5,0)
showbCTimePrec = showbPrec
{-# INLINE showbCTimePrec #-}
#else
showbCTimePrec = unsafeCoerce (showbPrec :: Int -> HTYPE_TIME_T -> Builder)
#endif

#if MIN_VERSION_base(4,4,0)
-- | Convert a 'CUSeconds' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
showbCUSeconds :: CUSeconds -> Builder
# if MIN_VERSION_base(4,5,0)
showbCUSeconds = showb
{-# INLINE showbCUSeconds #-}
# else
showbCUSeconds = unsafeCoerce (showb :: HTYPE_USECONDS_T -> Builder)
# endif

-- | Convert a 'CSUSeconds' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
showbCSUSecondsPrec :: Int -> CSUSeconds -> Builder
# if MIN_VERSION_base(4,5,0)
showbCSUSecondsPrec = showbPrec
{-# INLINE showbCSUSecondsPrec #-}
# else
showbCSUSecondsPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SUSECONDS_T -> Builder)
# endif
#endif

-- | Convert a 'CFloat' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCFloatPrec :: Int -> CFloat -> Builder
#if MIN_VERSION_base(4,5,0)
showbCFloatPrec = showbPrec
{-# INLINE showbCFloatPrec #-}
#else
showbCFloatPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_FLOAT -> Builder)
#endif

-- | Convert a 'CDouble' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCDoublePrec :: Int -> CDouble -> Builder
#if MIN_VERSION_base(4,5,0)
showbCDoublePrec = showbPrec
{-# INLINE showbCDoublePrec #-}
#else
showbCDoublePrec = unsafeCoerce (showbPrec :: Int -> HTYPE_DOUBLE -> Builder)
#endif

#if MIN_VERSION_base(4,5,0)
deriving instance TextShow CChar
deriving instance TextShow CSChar
deriving instance TextShow CUChar
deriving instance TextShow CShort
deriving instance TextShow CUShort
deriving instance TextShow CInt
deriving instance TextShow CUInt
deriving instance TextShow CLong
deriving instance TextShow CULong
deriving instance TextShow CPtrdiff
deriving instance TextShow CSize
deriving instance TextShow CWchar
deriving instance TextShow CSigAtomic
deriving instance TextShow CLLong
deriving instance TextShow CULLong
deriving instance TextShow CIntPtr
deriving instance TextShow CUIntPtr
deriving instance TextShow CIntMax
deriving instance TextShow CUIntMax
deriving instance TextShow CClock
deriving instance TextShow CTime
# if MIN_VERSION_base(4,4,0)
deriving instance TextShow CUSeconds
deriving instance TextShow CSUSeconds
# endif
deriving instance TextShow CFloat
deriving instance TextShow CDouble
#else
instance TextShow CChar where
    showbPrec = showbCCharPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CSChar where
    showbPrec = showbCSCharPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CUChar where
    showb = showbCUChar
    INLINE_INST_FUN(showb)

instance TextShow CShort where
    showbPrec = showbCShortPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CUShort where
    showb = showbCUShort
    INLINE_INST_FUN(showb)

instance TextShow CInt where
    showbPrec = showbCIntPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CUInt where
    showb = showbCUInt
    INLINE_INST_FUN(showb)

instance TextShow CLong where
    showbPrec = showbCLongPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CULong where
    showb = showbCULong
    INLINE_INST_FUN(showb)

instance TextShow CPtrdiff where
    showbPrec = showbCPtrdiffPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CSize where
    showb = showbCSize
    INLINE_INST_FUN(showb)

instance TextShow CWchar where
    showbPrec = showbCWcharPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CSigAtomic where
    showbPrec = showbCSigAtomicPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CLLong where
    showbPrec = showbCLLongPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CULLong where
    showb = showbCULLong
    INLINE_INST_FUN(showb)

instance TextShow CIntPtr where
    showbPrec = showbCIntPtrPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CUIntPtr where
    showb = showbCUIntPtr
    INLINE_INST_FUN(showb)

instance TextShow CIntMax where
    showbPrec = showbCIntMaxPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CUIntMax where
    showb = showbCUIntMax
    INLINE_INST_FUN(showb)

instance TextShow CClock where
    showbPrec = showbCClockPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CTime where
    showbPrec = showbCTimePrec
    INLINE_INST_FUN(showbPrec)

# if MIN_VERSION_base(4,4,0)
instance TextShow CUSeconds where
    showb = showbCUSeconds
    INLINE_INST_FUN(showb)

instance TextShow CSUSeconds where
    showbPrec = showbCSUSecondsPrec
    INLINE_INST_FUN(showbPrec)
# endif

instance TextShow CFloat where
    showbPrec = showbCFloatPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow CDouble where
    showbPrec = showbCDoublePrec
    INLINE_INST_FUN(showbPrec)
#endif
