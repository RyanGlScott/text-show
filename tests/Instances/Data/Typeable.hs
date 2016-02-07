{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Typeable
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Data.Typeable" module.
-}
module Instances.Data.Typeable () where

#include "MachDeps.h"

#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TypeRep(..))
import Instances.Utils ((<@>))
#else
import Data.Typeable (TyCon, TypeRep, mkTyCon, typeOf)
import Test.QuickCheck (Gen)
#endif

#if MIN_VERSION_base(4,9,0)
import GHC.Types (TyCon(..), TrName(..), Module(..))
# if WORD_SIZE_IN_BITS < 64
import GHC.Word (Word64(..))
# else
import GHC.Word (Word(..))
# endif
import Test.QuickCheck (oneof)
#elif MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TyCon(..))
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
#if MIN_VERSION_base(4,9,0)
    arbitrary = do
# if WORD_SIZE_IN_BITS < 64
        W64# w1# <- arbitrary
        W64# w2# <- arbitrary
# else
        W#   w1# <- arbitrary
        W#   w2# <- arbitrary
# endif
        TyCon w1# w2# <$> arbitrary <*> arbitrary
#elif MIN_VERSION_base(4,4,0)
    arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#else
    arbitrary = mkTyCon <$> arbitrary
#endif

#if MIN_VERSION_base(4,9,0)
instance Arbitrary TrName where
    arbitrary = oneof [pure (TrNameS "wat"#), TrNameD <$> arbitrary]

instance Arbitrary Module where
    arbitrary = Module <$> arbitrary <*> arbitrary
#endif
