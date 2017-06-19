{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

{-|
Module:      Instances.GHC.Stats
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'GCStats'.
-}
module Instances.GHC.Stats () where

import GHC.Generics (Generic)
import GHC.Stats (GCStats(..))

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

deriving instance Generic GCStats
instance Arbitrary GCStats where
    arbitrary = genericArbitrary
