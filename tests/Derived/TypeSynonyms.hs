{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Derived.TypeSynonyms
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types that use type synonyms.
-}
module Derived.TypeSynonyms (TyCon(..), TyFamily(..)) where

import           Control.Monad.Trans.Instances ()

import           Data.Orphans ()

import           GHC.Generics (Generic, Generic1)

import           Prelude

import           Test.QuickCheck (Arbitrary)

import           Text.Show.Deriving (deriveShow1, deriveShow2)

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

-------------------------------------------------------------------------------

type FakeOut a = Int
type Id a = a
type Flip f a b = f b a

-------------------------------------------------------------------------------

newtype TyCon a b = TyCon
    ( Id (FakeOut (Id a))
    , Id (FakeOut (Id b))
    , Id (Flip Either (Id a) (Id Int))
    , Id (Flip Either (Id b) (Id a))
    )
  deriving ( Arbitrary
           , Show
           , Generic
           , Generic1
           )

-------------------------------------------------------------------------------

data family TyFamily y z :: *

newtype instance TyFamily a b = TyFamily
    ( Id (FakeOut (Id a))
    , Id (FakeOut (Id b))
    , Id (Flip Either (Id a) (Id Int))
    , Id (Flip Either (Id b) (Id a))
    )
  deriving ( Arbitrary
           , Show
           , Generic
           , Generic1
           )

-------------------------------------------------------------------------------

$(deriveShow1 ''TyCon)
$(deriveShow2 ''TyCon)

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

$(deriveShow1 'TyFamily)
$(deriveShow2 'TyFamily)

$(deriveTextShow  'TyFamily)
$(deriveTextShow1 'TyFamily)
$(deriveTextShow2 'TyFamily)
