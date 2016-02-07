{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Data
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Data.Data" module.
-}
module Instances.Data.Data () where

import Data.Data (Constr, ConstrRep(..), DataRep(..), DataType,
                  Fixity(..), mkConstr, mkDataType)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Constr where
    arbitrary = mkConstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ConstrRep where
    arbitrary = oneof [ AlgConstr   <$> arbitrary
                      , IntConstr   <$> arbitrary
                      , FloatConstr <$> arbitrary
                      , CharConstr  <$> arbitrary
                      ]

instance Arbitrary DataRep where
    arbitrary = oneof [ AlgRep <$> arbitrary
                      , pure IntRep
                      , pure FloatRep
                      , pure CharRep
                      , pure NoRep
                      ]

instance Arbitrary DataType where
    arbitrary = mkDataType <$> arbitrary <*> arbitrary

deriving instance Bounded Fixity
deriving instance Enum Fixity
instance Arbitrary Fixity where
    arbitrary = arbitraryBoundedEnum
