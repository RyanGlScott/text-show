{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.Fingerprint
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Fingerprint'.
-}
module Instances.GHC.Fingerprint () where

import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Generics (Generic)

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Fingerprint where
    arbitrary = genericArbitrary

deriving instance Generic Fingerprint
