{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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

import           GHC.Generics (Generic, Generic1)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1, deriveShow2)

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
           , Generic1
           )

-------------------------------------------------------------------------------

infixr 1 :., :..., :....
data TyConGADT a b where
    (:.)    ::           c ->       d        -> TyConGADT c d
    (:..)   ::           e ->       f        -> TyConGADT e f
    (:...)  ::           g ->       h -> Int -> TyConGADT g h
    (:....) :: { tcg1 :: i, tcg2 :: j }      -> TyConGADT i j
  deriving ( Show
           , Generic
           , Generic1
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
           , Generic
           , Generic1
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
           , Generic
           , Generic1
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
$(deriveShow2 ''TyConPlain)

$(deriveShow1 ''TyConGADT)
$(deriveShow2 ''TyConGADT)

$(deriveTextShow  ''TyConPlain)
$(deriveTextShow1 ''TyConPlain)
$(deriveTextShow2 ''TyConPlain)

$(deriveTextShow  ''TyConGADT)
$(deriveTextShow1 ''TyConGADT)
$(deriveTextShow2 ''TyConGADT)

$(deriveShow1 '(:#:))
$(deriveShow2 '(:$:))

$(deriveShow1 '(:*))
$(deriveShow2 '(:***))

$(deriveTextShow  '(:#:))
$(deriveTextShow1 '(:$:))
$(deriveTextShow2 'TyFamilyPlain)

$(deriveTextShow  '(:*))
$(deriveTextShow1 '(:***))
$(deriveTextShow2 '(:****))
