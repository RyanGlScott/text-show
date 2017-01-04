{-# LANGUAGE CPP                #-}

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

{-|
Module:      Instances.Data.OldTypeable
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Data.OldTypeable" module.
-}
module Instances.Data.OldTypeable () where

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import Data.OldTypeable.Internal (TyCon(..), TypeRep(..))

import GHC.Generics (Generic)

import Instances.GHC.Fingerprint ()
import Instances.Utils ((<@>))
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary TypeRep where
    arbitrary = TypeRep <$> arbitrary <*> arbitrary <@> []
--     arbitrary = TypeRep <$> arbitrary <*> arbitrary <*> arbitrary

deriving instance Generic TyCon
instance Arbitrary TyCon where
    arbitrary = genericArbitrary
#endif
