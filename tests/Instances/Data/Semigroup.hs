{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Semigroup
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for the 'Arg' datatype.
-}
module Instances.Data.Semigroup () where

import Data.Semigroup.Compat (Arg(..))
import Instances.Utils.GenericArbitrary (genericArbitrary)
import Test.QuickCheck (Arbitrary(..))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Arg a b) where
    arbitrary = genericArbitrary
