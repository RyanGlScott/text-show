{-# LANGUAGE CPP                #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.StaticPtr
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'StaticPtrInfo'.
-}
module Instances.GHC.StaticPtr () where

#if MIN_VERSION_base(4,8,0)
import GHC.Generics (Generic)
import GHC.StaticPtr (StaticPtrInfo(..))

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

deriving instance Generic StaticPtrInfo
instance Arbitrary StaticPtrInfo where
    arbitrary = genericArbitrary
#endif
