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

import Data.Int
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Word

import Foreign.C.Types
import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr,
                    castPtrToFunPtr, nullPtr, plusPtr,
                    ptrToIntPtr, ptrToWordPtr)

import Test.QuickCheck

#include "HsBaseConfig.h"

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