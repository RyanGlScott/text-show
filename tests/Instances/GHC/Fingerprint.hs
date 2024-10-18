{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
#if !(MIN_VERSION_base(4,15,0))
import GHC.Generics (Generic)
#endif

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Fingerprint where
    arbitrary = genericArbitrary

#if !(MIN_VERSION_base(4,15,0))
deriving instance Generic Fingerprint
#endif
