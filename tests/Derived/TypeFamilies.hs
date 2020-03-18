-- {-# LANGUAGE CPP                        #-}
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

{-
import Text.Show.Deriving (deriveShow1)
#if defined(NEW_FUNCTOR_CLASSES)
import Text.Show.Deriving (deriveShow2)
#endif
-}

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

{-
TODO: Define these once we depend on deriving-compat-0.5.8 as the minimum

$(deriveShow1 ''TyConOverSat)
$(deriveShow1 'TyFamilyOverSat)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyConOverSat)
$(deriveShow2 'TyFamilyOverSat)
#endif
-}

$(deriveTextShow  ''TyConOverSat)
$(deriveTextShow1 ''TyConOverSat)
$(deriveTextShow2 ''TyConOverSat)

$(deriveTextShow  'TyFamilyOverSat)
$(deriveTextShow1 'TyFamilyOverSat)
$(deriveTextShow2 'TyFamilyOverSat)

{-
TODO: Define Generic(1) instances once we depend on generic-deriving-1.13.2
as the minimum
-}
