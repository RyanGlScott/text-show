{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Foreign.Ptr
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for pointer types used in the Haskell
Foreign Function Interface (FFI).

/Since: 2/
-}
module TextShow.Foreign.Ptr () where

import Data.Semigroup.Compat (mtimesDefault)
import Data.Text.Lazy.Builder (Builder, singleton)

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (FunPtr, IntPtr, WordPtr, castFunPtrToPtr)

import GHC.Exts (addr2Int#, int2Word#)
import GHC.ForeignPtr (unsafeForeignPtrToPtr)
import GHC.Num (wordToInteger)
import GHC.Ptr (Ptr(..))

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..), TextShow1(..))
import TextShow.Data.Integral (showbHex)
import TextShow.Utils (lengthB)

import Unsafe.Coerce (unsafeCoerce)

#include "MachDeps.h"

-- | /Since: 2/
instance TextShow (Ptr a) where
    showbPrec = liftShowbPrec undefined undefined
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 Ptr where
    liftShowbPrec _ _ _ (Ptr a) = padOut . showbHex $ wordToInteger (int2Word# (addr2Int# a))
      where
        padOut :: Builder -> Builder
        padOut ls =
             singleton '0' <> singleton 'x'
          <> mtimesDefault (max 0 $ 2*SIZEOF_HSPTR - lengthB ls) (singleton '0')
          <> ls

-- | /Since: 2/
instance TextShow (FunPtr a) where
    showbPrec = liftShowbPrec undefined undefined
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 FunPtr where
    liftShowbPrec _ _ _ = showb . castFunPtrToPtr
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow IntPtr where
    showbPrec p ip = showbPrec p (unsafeCoerce ip :: Integer)

-- | /Since: 2/
instance TextShow WordPtr where
    showb wp = showb (unsafeCoerce wp :: Word)

-- | /Since: 2/
instance TextShow (ForeignPtr a) where
    showbPrec = liftShowbPrec undefined undefined
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 ForeignPtr where
    liftShowbPrec _ _ _ = showb . unsafeForeignPtrToPtr
    {-# INLINE liftShowbPrec #-}
