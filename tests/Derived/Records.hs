{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module:      Derived.Records
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with record syntax.
-}
module Derived.Records (TyCon(..), TyFamily(..)) where

#include "generic.h"

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Generics (Generic, Generic1)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1)
#if defined(NEW_FUNCTOR_CLASSES)
import           Text.Show.Deriving (deriveShow2)
#endif

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

-------------------------------------------------------------------------------

infixl 4 :@:
data TyCon a b = TyConPrefix { tc1 :: a, tc2  :: b }
               | (:@:)       { tc3 :: b, (##) :: a }
  deriving ( Show
           , Generic
           , Generic1
           )

-------------------------------------------------------------------------------

data family TyFamily y z :: *

infixl 4 :!:
data instance TyFamily a b = TyFamilyPrefix { tf1 :: a, tf2   :: b }
                           | (:!:)          { tf3 :: b, (###) :: a }
  deriving ( Show
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
           )

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon a b) where
    arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily a b) where
    arbitrary = genericArbitrary

-------------------------------------------------------------------------------

$(deriveShow1 ''TyCon)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyCon)
#endif

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'TyFamilyPrefix)
#else
$(deriveShow1 'TyFamilyPrefix)
$(deriveShow2 '(:!:))
#endif

$(deriveTextShow  'TyFamilyPrefix)
$(deriveTextShow1 '(:!:))
$(deriveTextShow2 'TyFamilyPrefix)

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'TyFamilyPrefix)
$(Generics.deriveRepresentable1 '(:!:))
#endif
