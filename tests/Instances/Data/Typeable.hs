{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Typeable
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instances for data types in the "Data.Typeable" module.
-}
module Instances.Data.Typeable () where

#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TyCon(..), TypeRep(..))
import Instances.Utils ((<@>))
#else
import Data.Typeable (TyCon, TypeRep, mkTyCon, typeOf)
import Test.QuickCheck (Gen)
#endif

import Instances.GHC.Fingerprint ()

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary TypeRep where
#if MIN_VERSION_base(4,4,0)
    arbitrary = TypeRep <$> arbitrary
                        <*> arbitrary
# if MIN_VERSION_base(4,8,0)
                        <@> [] <@> []
# else
                        <@> []
# endif
#else
    arbitrary = typeOf <$> (arbitrary :: Gen Int)
#endif

instance Arbitrary TyCon where
#if MIN_VERSION_base(4,4,0)
    arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#else
    arbitrary = mkTyCon <$> arbitrary
#endif
