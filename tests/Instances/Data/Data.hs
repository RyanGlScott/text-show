{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Data
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Data.Data" module.
-}
module Instances.Data.Data () where

import Data.Data (Constr, ConstrRep(..), DataRep(..), DataType,
                  Fixity(..), mkConstr, mkDataType)

import GHC.Generics (Generic)

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary Constr where
    arbitrary = mkConstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ConstrRep where
    arbitrary = genericArbitrary

instance Arbitrary DataRep where
    arbitrary = genericArbitrary

instance Arbitrary DataType where
    arbitrary = mkDataType <$> arbitrary <*> arbitrary

deriving instance Bounded Fixity
deriving instance Enum Fixity
instance Arbitrary Fixity where
    arbitrary = arbitraryBoundedEnum

deriving instance Generic ConstrRep
deriving instance Generic DataRep
