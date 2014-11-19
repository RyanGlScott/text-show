{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
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
-- Miscellaneous typeclass instances.
----------------------------------------------------------------------------
module Instances where

#if MIN_VERSION_bytestring(0,10,4)
import Data.ByteString.Short (ShortByteString, pack)
#endif
import Data.Monoid (All(..), Any(..), Dual(..), First(..),
                    Last(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#endif
import Data.Text.Lazy.Builder (Builder, fromString)

import Foreign.C.Types
import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr,
                    castPtrToFunPtr, nullPtr, plusPtr,
                    ptrToIntPtr, ptrToWordPtr)

import Test.QuickCheck

instance Arbitrary Builder where
    arbitrary = fmap fromString arbitrary

#if MIN_VERSION_bytestring(0,10,4)
instance Arbitrary ShortByteString where
    arbitrary = fmap pack arbitrary
#endif

instance Arbitrary (Ptr a) where
    arbitrary = fmap (plusPtr nullPtr) arbitrary

instance Arbitrary (FunPtr a) where
    arbitrary = fmap castPtrToFunPtr arbitrary

instance Arbitrary IntPtr where
    arbitrary = fmap ptrToIntPtr arbitrary

instance Arbitrary WordPtr where
    arbitrary = fmap ptrToWordPtr arbitrary

deriving instance Arbitrary CChar
deriving instance Arbitrary CSChar
deriving instance Arbitrary CUChar
deriving instance Arbitrary CShort
deriving instance Arbitrary CUShort
deriving instance Arbitrary CInt
deriving instance Arbitrary CUInt
deriving instance Arbitrary CLong
deriving instance Arbitrary CULong
deriving instance Arbitrary CLLong
deriving instance Arbitrary CULLong
deriving instance Arbitrary CFloat
deriving instance Arbitrary CDouble
deriving instance Arbitrary CPtrdiff
deriving instance Arbitrary CSize
deriving instance Arbitrary CWchar
deriving instance Arbitrary CSigAtomic
deriving instance Arbitrary CClock
deriving instance Arbitrary CTime
deriving instance Arbitrary CUSeconds
deriving instance Arbitrary CSUSeconds
deriving instance Arbitrary CIntPtr
deriving instance Arbitrary CUIntPtr
deriving instance Arbitrary CIntMax
deriving instance Arbitrary CUIntMax
deriving instance Arbitrary All
deriving instance Arbitrary Any
deriving instance Arbitrary a => Arbitrary (Dual a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary a => Arbitrary (Product a)
deriving instance Arbitrary a => Arbitrary (Sum a)

#if MIN_VERSION_base(4,6,0)
deriving instance Arbitrary a => Arbitrary (Down a)
#if !MIN_VERSION_base(4,7,0)
deriving instance Show a => Show (Down a)
#endif
#endif