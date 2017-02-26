{-# LANGUAGE CPP                        #-}
#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Foreign.C.Types
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for Haskell newtypes corresponding to C
types in the Foreign Function Interface (FFI).

/Since: 2/
-}
module TextShow.Foreign.C.Types () where

import Foreign.C.Types

import TextShow.Classes (TextShow(..))
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()

#if !(MIN_VERSION_base(4,5,0))
import Data.Int
import Data.Text.Lazy.Builder (Builder)
import Data.Word

import Unsafe.Coerce (unsafeCoerce)

# include "HsBaseConfig.h"
#endif

#if MIN_VERSION_base(4,5,0)
-- | /Since: 2/
deriving instance TextShow CChar
-- | /Since: 2/
deriving instance TextShow CSChar
-- | /Since: 2/
deriving instance TextShow CUChar
-- | /Since: 2/
deriving instance TextShow CShort
-- | /Since: 2/
deriving instance TextShow CUShort
-- | /Since: 2/
deriving instance TextShow CInt
-- | /Since: 2/
deriving instance TextShow CUInt
-- | /Since: 2/
deriving instance TextShow CLong
-- | /Since: 2/
deriving instance TextShow CULong
-- | /Since: 2/
deriving instance TextShow CPtrdiff
-- | /Since: 2/
deriving instance TextShow CSize
-- | /Since: 2/
deriving instance TextShow CWchar
-- | /Since: 2/
deriving instance TextShow CSigAtomic
-- | /Since: 2/
deriving instance TextShow CLLong
-- | /Since: 2/
deriving instance TextShow CULLong
-- | /Since: 2/
deriving instance TextShow CIntPtr
-- | /Since: 2/
deriving instance TextShow CUIntPtr
-- | /Since: 2/
deriving instance TextShow CIntMax
-- | /Since: 2/
deriving instance TextShow CUIntMax
-- | /Since: 2/
deriving instance TextShow CClock
-- | /Since: 2/
deriving instance TextShow CTime
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
deriving instance TextShow CUSeconds
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
deriving instance TextShow CSUSeconds
-- | /Since: 2/
deriving instance TextShow CFloat
-- | /Since: 2/
deriving instance TextShow CDouble
#else
-- | /Since: 2/
instance TextShow CChar where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_CHAR -> Builder)

-- | /Since: 2/
instance TextShow CSChar where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SIGNED_CHAR -> Builder)

-- | /Since: 2/
instance TextShow CUChar where
    showb = unsafeCoerce (showb :: HTYPE_UNSIGNED_CHAR -> Builder)

-- | /Since: 2/
instance TextShow CShort where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SHORT -> Builder)

-- | /Since: 2/
instance TextShow CUShort where
    showb = unsafeCoerce (showb :: HTYPE_UNSIGNED_SHORT -> Builder)

-- | /Since: 2/
instance TextShow CInt where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_INT -> Builder)

-- | /Since: 2/
instance TextShow CUInt where
    showb = unsafeCoerce (showb :: HTYPE_UNSIGNED_INT -> Builder)

-- | /Since: 2/
instance TextShow CLong where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_LONG -> Builder)

-- | /Since: 2/
instance TextShow CULong where
    showb = unsafeCoerce (showb :: HTYPE_UNSIGNED_LONG -> Builder)

-- | /Since: 2/
instance TextShow CPtrdiff where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_PTRDIFF_T -> Builder)

-- | /Since: 2/
instance TextShow CSize where
    showb = unsafeCoerce (showb :: HTYPE_SIZE_T -> Builder)

-- | /Since: 2/
instance TextShow CWchar where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_WCHAR_T -> Builder)

-- | /Since: 2/
instance TextShow CSigAtomic where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SIG_ATOMIC_T -> Builder)

-- | /Since: 2/
instance TextShow CLLong where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_LONG_LONG -> Builder)

-- | /Since: 2/
instance TextShow CULLong where
    showb = unsafeCoerce (showb :: HTYPE_UNSIGNED_LONG_LONG -> Builder)

-- | /Since: 2/
instance TextShow CIntPtr where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_INTPTR_T -> Builder)

-- | /Since: 2/
instance TextShow CUIntPtr where
    showb = unsafeCoerce (showb :: HTYPE_UINTPTR_T -> Builder)

-- | /Since: 2/
instance TextShow CIntMax where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_INTMAX_T -> Builder)

-- | /Since: 2/
instance TextShow CUIntMax where
    showb = unsafeCoerce (showb :: HTYPE_UINTMAX_T -> Builder)

-- | /Since: 2/
instance TextShow CClock where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_CLOCK_T -> Builder)

-- | /Since: 2/
instance TextShow CTime where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_TIME_T -> Builder)

# if MIN_VERSION_base(4,4,0)
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
instance TextShow CUSeconds where
    showb = unsafeCoerce (showb :: HTYPE_USECONDS_T -> Builder)

-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
instance TextShow CSUSeconds where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SUSECONDS_T -> Builder)
# endif

-- | /Since: 2/
instance TextShow CFloat where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_FLOAT -> Builder)

-- | /Since: 2/
instance TextShow CDouble where
    showbPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_DOUBLE -> Builder)
#endif
