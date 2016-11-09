{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

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

import           Data.Data (Constr, ConstrRep(..), DataRep(..), DataType,
                            Fixity(..), mkConstr, mkDataType)

#if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

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

#if __GLASGOW_HASKELL__ >= 704
deriving instance Generic ConstrRep
deriving instance Generic DataRep
#else
$(Generics.deriveAll0 ''ConstrRep)
$(Generics.deriveAll0 ''DataRep)
#endif
