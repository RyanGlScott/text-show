{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-|
Module:      Derived.RankNTypes
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with rank-n voodoo.
-}
module Derived.RankNTypes (TyCon(..), TyFamily(..)) where

import Data.Functor.Classes (Show1(..), Show2(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import Text.Show.Deriving (deriveShow1, deriveShow2,
                           makeLiftShowsPrec, makeLiftShowsPrec2)
import TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2,
                    makeShowbPrec, makeLiftShowbPrec, makeLiftShowbPrec2)

-------------------------------------------------------------------------------

data TyCon a b = TyCon (forall a. Tagged2 a Int b)
                       (forall b. Tagged2 b a   a)

deriving instance (Show a, Show b) => Show (TyCon a b)

-------------------------------------------------------------------------------

data family TyFamily x y :: *

data instance TyFamily a b = TyFamily (forall a. Tagged2 a Int b)
                                      (forall b. Tagged2 b a   a)

deriving instance (Show a, Show b) => Show (TyFamily a b)

-------------------------------------------------------------------------------

newtype Tagged2 s t c = Tagged2 c
  deriving Show

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon a b) where
    arbitrary = (\i1 i2 -> TyCon (Tagged2 i1) (Tagged2 i2))
                    <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily a b) where
    arbitrary = (\i1 i2 -> TyFamily (Tagged2 i1) (Tagged2 i2))
                    <$> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

$(return [])

instance TextShow c => TextShow (Tagged2 s t c) where
    showbPrec = $(makeShowbPrec ''Tagged2)

instance TextShow1 (Tagged2 s t) where
    liftShowbPrec = $(makeLiftShowbPrec ''Tagged2)

instance TextShow2 (Tagged2 s) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 ''Tagged2)

-------------------------------------------------------------------------------

instance Show1 (Tagged2 s t) where
    liftShowsPrec = $(makeLiftShowsPrec ''Tagged2)
instance Show2 (Tagged2 s) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 ''Tagged2)

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
