{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-|
Module:      Derived.TypeFamilies
Copyright:   (C) 2020 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines corner case-provoking type families.
-}
module Derived.TypeFamilies (
      TyConOverSat(..)
    , TyFamilyOverSat(..)
    ) where

import Test.QuickCheck (Arbitrary)

import Text.Show.Deriving (deriveShow1, deriveShow2)

import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

-------------------------------------------------------------------------------

type family F :: * -> * -> *
type instance F = Either

newtype TyConOverSat a b = TyConOverSat (F a b)
  deriving (Arbitrary, Show)

data family TyFamilyOverSat (x :: *) (y :: *)
newtype instance TyFamilyOverSat a b = TyFamilyOverSat (F a b)
  deriving (Arbitrary, Show)

-------------------------------------------------------------------------------

$(deriveShow1 ''TyConOverSat)
$(deriveShow2 ''TyConOverSat)

$(deriveShow1 'TyFamilyOverSat)
$(deriveShow2 'TyFamilyOverSat)

$(deriveTextShow  ''TyConOverSat)
$(deriveTextShow1 ''TyConOverSat)
$(deriveTextShow2 ''TyConOverSat)

$(deriveTextShow  'TyFamilyOverSat)
$(deriveTextShow1 'TyFamilyOverSat)
$(deriveTextShow2 'TyFamilyOverSat)
