{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Options
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Options' and related datatypes.
-}
module Instances.Options () where

import Instances.Utils.GenericArbitrary (genericArbitrary)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)
import TextShow.TH (Options(..), GenTextMethods)

instance Arbitrary Options where
    arbitrary = genericArbitrary

instance Arbitrary GenTextMethods where
    arbitrary = arbitraryBoundedEnum
