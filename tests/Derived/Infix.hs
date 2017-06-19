{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds         #-}
#endif

{-|
Module:      Derived.Infix
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with infix constructors.
-}
module Derived.Infix (
      TyConPlain(..)
    , TyConGADT(..)
    , TyFamilyPlain(..)
    , TyFamilyGADT(..)
    ) where

#include "generic.h"

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
#endif

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

infixl 3 :!:
infix  4 :@:
infixr 5 `TyConPlain`
infixr 6 `TyConFakeInfix`
data TyConPlain a b = (:!:) a b
                    | a :@: b
                    | a `TyConPlain` b
                    | TyConFakeInfix a b
  deriving ( Show
           , Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

-------------------------------------------------------------------------------

infixr 1 :., :..., :....
data TyConGADT a b where
    (:.)    ::           c ->       d        -> TyConGADT c d
    (:..)   ::           e ->       f        -> TyConGADT e f
    (:...)  ::           g ->       h -> Int -> TyConGADT g h
    (:....) :: { tcg1 :: i, tcg2 :: j }      -> TyConGADT i j
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
           , Generic1
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyPlain y z :: *

infixl 3 :#:
infix  4 :$:
infixr 5 `TyFamilyPlain`
infixr 6 `TyFamilyFakeInfix`
data instance TyFamilyPlain a b = (:#:) a b
                                | a :$: b
                                | a `TyFamilyPlain` b
                                | TyFamilyFakeInfix a b
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyGADT y z :: *

infixr 1 :*, :***, :****
data instance TyFamilyGADT a b where
    (:*)    ::           c ->       d        -> TyFamilyGADT c d
    (:**)   ::           e ->       f        -> TyFamilyGADT e f
    (:***)  ::           g ->       h -> Int -> TyFamilyGADT g h
    (:****) :: { tfg1 :: i, tfg2 :: j }      -> TyFamilyGADT i j
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyConPlain a b) where
    arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyConGADT a b) where
    arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamilyPlain a b) where
    arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamilyGADT a b) where
    arbitrary = genericArbitrary

-------------------------------------------------------------------------------

$(deriveShow1 ''TyConPlain)
$(deriveShow1 ''TyConGADT)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyConPlain)
$(deriveShow2 ''TyConGADT)
#endif

$(deriveTextShow  ''TyConPlain)
$(deriveTextShow1 ''TyConPlain)
$(deriveTextShow2 ''TyConPlain)

$(deriveTextShow  ''TyConGADT)
$(deriveTextShow1 ''TyConGADT)
$(deriveTextShow2 ''TyConGADT)

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveMeta           ''TyConPlain)
$(Generics.deriveRepresentable1 ''TyConPlain)
$(Generics.deriveAll0And1       ''TyConGADT)
#endif

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 '(:#:))

$(deriveShow1 '(:*))
#else
$(deriveShow1 '(:#:))
$(deriveShow2 '(:$:))

$(deriveShow1 '(:*))
$(deriveShow2 '(:***))
#endif

$(deriveTextShow  '(:#:))
$(deriveTextShow1 '(:$:))
$(deriveTextShow2 'TyFamilyPlain)

$(deriveTextShow  '(:*))
$(deriveTextShow1 '(:***))
$(deriveTextShow2 '(:****))

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           '(:#:))
$(Generics.deriveRepresentable1 '(:$:))
$(Generics.deriveMeta           '(:*))
$(Generics.deriveRepresentable1 '(:**))
#endif

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamilyPlain)
$(Generics.deriveRepresentable0 '(:***))
#endif
