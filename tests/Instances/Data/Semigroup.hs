{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Semigroup
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for datatypes in the "Data.Semigroup" module.
-}
module Instances.Data.Semigroup () where

import Data.Semigroup (Min(..), Max(..), First(..), Last(..),
                       WrappedMonoid(..), Option(..), Arg(..))
import Instances.Utils.GenericArbitrary (genericArbitrary)
import Test.QuickCheck (Arbitrary(..))

deriving instance Arbitrary a => Arbitrary (Min a)
deriving instance Arbitrary a => Arbitrary (Max a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary a => Arbitrary (WrappedMonoid a)
deriving instance Arbitrary a => Arbitrary (Option a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Arg a b) where
    arbitrary = genericArbitrary
