{-# LANGUAGE CPP                        #-}

#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Foreign.C.Types
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Foreign.C.Types" module.
-}
module Instances.Foreign.C.Types () where

import Foreign.C.Types
import Test.QuickCheck (Arbitrary(..))

#if !(MIN_VERSION_base(4,5,0))
import Data.Int
# if MIN_VERSION_base(4,4,0)
import Data.Word
# endif

import Test.QuickCheck (Gen, arbitrarySizedBoundedIntegral, arbitrarySizedFractional)
import Unsafe.Coerce (unsafeCoerce)

# include "HsBaseConfig.h"
#endif

#if MIN_VERSION_base(4,5,0)
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
# if MIN_VERSION_base(4,4,0)
deriving instance Arbitrary CUSeconds
deriving instance Arbitrary CSUSeconds
# endif
deriving instance Arbitrary CIntPtr
deriving instance Arbitrary CUIntPtr
deriving instance Arbitrary CIntMax
deriving instance Arbitrary CUIntMax
#else
instance Arbitrary CChar where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CSChar where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CUChar where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CShort where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CUShort where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CInt where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CUInt where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CLong where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CULong where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CLLong where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CULLong where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CFloat where
    arbitrary = arbitrarySizedFractional

instance Arbitrary CDouble where
    arbitrary = arbitrarySizedFractional

instance Arbitrary CPtrdiff where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CSize where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CWchar where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CSigAtomic where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CClock where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_CLOCK_T)

instance Arbitrary CTime where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_TIME_T)

# if MIN_VERSION_base(4,4,0)
instance Arbitrary CUSeconds where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_USECONDS_T)

instance Arbitrary CSUSeconds where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_SUSECONDS_T)
# endif

instance Arbitrary CIntPtr where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CUIntPtr where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CIntMax where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary CUIntMax where
    arbitrary = arbitrarySizedBoundedIntegral
#endif
