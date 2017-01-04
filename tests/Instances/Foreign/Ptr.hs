{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Foreign.Ptr
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for pointer data types.
-}
module Instances.Foreign.Ptr () where

import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr,
                    castPtrToFunPtr, nullPtr, plusPtr,
                    ptrToIntPtr, ptrToWordPtr)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

instance Arbitrary (FunPtr a) where
    arbitrary = castPtrToFunPtr <$> arbitrary

instance Arbitrary IntPtr where
    arbitrary = ptrToIntPtr <$> arbitrary

instance Arbitrary WordPtr where
    arbitrary = ptrToWordPtr <$> arbitrary
