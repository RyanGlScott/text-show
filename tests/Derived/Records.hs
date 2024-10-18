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

import           GHC.Generics (Generic, Generic1)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1, deriveShow2)

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
           , Generic1
           )

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon a b) where
    arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily a b) where
    arbitrary = genericArbitrary

-------------------------------------------------------------------------------

$(deriveShow1 ''TyCon)
$(deriveShow2 ''TyCon)

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

$(deriveShow1 'TyFamilyPrefix)
$(deriveShow2 '(:!:))

$(deriveTextShow  'TyFamilyPrefix)
$(deriveTextShow1 '(:!:))
$(deriveTextShow2 'TyFamilyPrefix)
