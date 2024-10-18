{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

import GHC.Generics (Generic)
import GHC.StaticPtr (StaticPtrInfo(..))

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

deriving instance Generic StaticPtrInfo
instance Arbitrary StaticPtrInfo where
    arbitrary = genericArbitrary
