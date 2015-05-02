{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.OldTypeable
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instances for data types in the "Data.OldTypeable" module.
-}
module Instances.Data.OldTypeable () where

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import Data.OldTypeable.Internal (TyCon(..), TypeRep(..))

import Instances.Utils ((<@>))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary TypeRep where
    arbitrary = TypeRep <$> arbitrary <*> arbitrary <@> []
--     arbitrary = TypeRep <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TyCon where
    arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#endif
