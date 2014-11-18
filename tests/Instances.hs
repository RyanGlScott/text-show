{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Instances
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Miscellaneous 'Arbitrary' instances.
----------------------------------------------------------------------------
module Instances where

#if MIN_VERSION_bytestring(0,10,4)
import Data.ByteString.Short (ShortByteString, pack)
#endif
import Data.Int
import Data.Ord (Down(..))
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Word

import Foreign.C.Types
import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr,
                    castPtrToFunPtr, nullPtr, plusPtr,
                    ptrToIntPtr, ptrToWordPtr)

import Test.QuickCheck

#include "HsBaseConfig.h"

instance Arbitrary a => Arbitrary (Down a) where
    arbitrary = fmap Down arbitrary

#if MIN_VERSION_bytestring(0,10,4)
instance Arbitrary ShortByteString where
    arbitrary = fmap pack arbitrary
#endif

instance Arbitrary Builder where
    arbitrary = fmap fromString arbitrary

instance Arbitrary (Ptr a) where
    arbitrary = fmap (plusPtr nullPtr) arbitrary

instance Arbitrary (FunPtr a) where
    arbitrary = fmap castPtrToFunPtr arbitrary

instance Arbitrary IntPtr where
    arbitrary = fmap ptrToIntPtr arbitrary

instance Arbitrary WordPtr where
    arbitrary = fmap ptrToWordPtr arbitrary

instance Arbitrary CChar where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_CHAR)

instance Arbitrary CSChar where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_SIGNED_CHAR)

instance Arbitrary CUChar where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_UNSIGNED_CHAR)

instance Arbitrary CShort where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_SHORT)

instance Arbitrary CUShort where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_UNSIGNED_SHORT)

instance Arbitrary CInt where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_INT)

instance Arbitrary CUInt where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_UNSIGNED_INT)

instance Arbitrary CLong where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_LONG)

instance Arbitrary CULong where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_UNSIGNED_LONG)

instance Arbitrary CLLong where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_LONG_LONG)

instance Arbitrary CULLong where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_UNSIGNED_LONG_LONG)

instance Arbitrary CFloat where
    arbitrary = fmap realToFrac (arbitrary :: Gen HTYPE_FLOAT)

instance Arbitrary CDouble where
    arbitrary = fmap realToFrac (arbitrary :: Gen HTYPE_DOUBLE)

instance Arbitrary CPtrdiff where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_PTRDIFF_T)

instance Arbitrary CSize where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_SIZE_T)

instance Arbitrary CWchar where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_WCHAR_T)

instance Arbitrary CSigAtomic where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_SIG_ATOMIC_T)

instance Arbitrary CClock where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_CLOCK_T)

instance Arbitrary CTime where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_TIME_T)

instance Arbitrary CUSeconds where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_USECONDS_T)

instance Arbitrary CSUSeconds where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_SUSECONDS_T)

instance Arbitrary CIntPtr where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_INTPTR_T)

instance Arbitrary CUIntPtr where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_INTMAX_T)

instance Arbitrary CIntMax where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_INTMAX_T)

instance Arbitrary CUIntMax where
    arbitrary = fmap fromIntegral (arbitrary :: Gen HTYPE_UINTMAX_T)