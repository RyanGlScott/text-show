{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Generic
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'ConType'.
-}
module Instances.Generic () where

import Instances.Data.Text ()
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

import TextShow.Generic (ConType(..))

instance Arbitrary ConType where
    arbitrary = genericArbitrary
