{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Typeable
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Data.Typeable" module.
-}
module Instances.Data.Typeable () where

#include "MachDeps.h"

#if MIN_VERSION_base(4,9,0)
import GHC.Types (TyCon(..), TrName(..), Module(..))
# if WORD_SIZE_IN_BITS < 64
import GHC.Word (Word64(..))
# else
import GHC.Word (Word(..))
# endif
#else
import Data.Typeable.Internal (TyCon(..))
#endif

#if MIN_VERSION_base(4,10,0)
import GHC.Exts (Int(..), Ptr(..))
import GHC.Types (KindRep(..), RuntimeRep(..), TypeLitSort(..),
                  VecCount(..), VecElem(..))
import Type.Reflection (SomeTypeRep(..), Typeable, TypeRep, typeRep)
#else
import Data.Typeable.Internal (TypeRep(..))
#endif

import Instances.Foreign.Ptr ()
import Instances.GHC.Fingerprint ()
import Instances.Utils ((<@>))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

#if MIN_VERSION_base(4,10,0)
instance Typeable a => Arbitrary (TypeRep (a :: k)) where
    arbitrary = pure (typeRep :: TypeRep (a :: k))

instance Arbitrary SomeTypeRep where
    arbitrary = SomeTypeRep <$> (arbitrary :: Gen (TypeRep Int))

deriving instance Bounded TypeLitSort
deriving instance Enum TypeLitSort
instance Arbitrary TypeLitSort where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary KindRep where
    arbitrary = oneof [ KindRepTyConApp <$> arbitrary <@> []
                      , KindRepVar <$> arbitrary
                      , KindRepApp <$> krt <*> krt
                      , krt
                      , do Ptr a# <- arbitrary
                           (\a -> KindRepTypeLitS a a#) <$> arbitrary
                      , KindRepTypeLitD <$> arbitrary <*> arbitrary
                      ]
      where
        krt = KindRepTYPE <$> arbitrary

instance Arbitrary RuntimeRep where
    arbitrary = oneof [ VecRep <$> arbitrary <*> arbitrary
                      , pure $ TupleRep []
                      , pure $ SumRep []
                      , pure LiftedRep
                      , pure UnliftedRep
                      , pure IntRep
                      , pure WordRep
                      , pure Int64Rep
                      , pure Word64Rep
                      , pure AddrRep
                      , pure FloatRep
                      , pure DoubleRep
                      ]

instance Arbitrary VecCount where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary VecElem where
    arbitrary = arbitraryBoundedEnum
#else /* !(MIN_VERSION_base(4,10,0) */
instance Arbitrary TypeRep where
    arbitrary = TypeRep <$> arbitrary
                        <*> arbitrary
# if MIN_VERSION_base(4,8,0)
                        <@> [] <@> []
# else
                        <@> []
# endif
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
# if MIN_VERSION_base(4,10,0)
        I# i# <- arbitrary
        (\a1 a2 a3 -> TyCon w1# w2# a1 a2 i# a3)
            <$> arbitrary <*> arbitrary <*> arbitrary
# else
        TyCon w1# w2# <$> arbitrary <*> arbitrary
# endif
#else
    arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#endif

#if MIN_VERSION_base(4,9,0)
instance Arbitrary TrName where
    arbitrary = oneof [pure (TrNameS "wat"#), TrNameD <$> arbitrary]

instance Arbitrary Module where
    arbitrary = Module <$> arbitrary <*> arbitrary
#endif
