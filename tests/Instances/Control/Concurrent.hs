{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Control.Concurrent
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Control.Concurrent" module.
-}
module Instances.Control.Concurrent () where

import GHC.Conc (BlockReason(..), ThreadStatus(..))
import GHC.Generics (Generic)

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded BlockReason
deriving instance Enum BlockReason
instance Arbitrary BlockReason where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ThreadStatus where
    arbitrary = genericArbitrary

deriving instance Generic ThreadStatus
