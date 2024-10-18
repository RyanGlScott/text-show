{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-|
Module:      Derived.DataFamilies
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines corner case-provoking data families.
-}
module Derived.DataFamilies (
      NotAllShow(..)
    , KindDistinguished(..)
    , NullaryClass(..)
    , NullaryData(..)
    ) where

import           GHC.Generics (Generic, Generic1)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1, deriveShow2)

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

-------------------------------------------------------------------------------

data family NotAllShow (w :: *) (x :: *) (y :: *) (z :: *) :: *

data instance NotAllShow ()  ()  () d = NASNoShow
data instance NotAllShow Int b   c  d = NASShow1 c b
                                      | NASShow2 d
  deriving ( Show
           , Generic
           , Generic1
           )

instance (Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (NotAllShow Int b c d) where
    arbitrary = genericArbitrary

$(deriveShow1 'NASShow1)
$(deriveShow2 'NASShow2)

$(deriveTextShow  'NASShow1)
$(deriveTextShow1 'NASShow2)
$(deriveTextShow2 'NASShow1)

-------------------------------------------------------------------------------

data family KindDistinguished (x :: k) (y :: *) (z :: *) :: *

data instance KindDistinguished (a :: ()) b c = KindDistinguishedUnit b c
  deriving ( Show
           , Generic
           , Generic1
           )

data instance KindDistinguished (a :: Bool) b c = KindDistinguishedBool b c
  deriving ( Show
           , Generic
           , Generic1
           )

instance (Arbitrary b, Arbitrary c)
      => Arbitrary (KindDistinguished (a :: ()) b c) where
    arbitrary = genericArbitrary

instance (Arbitrary b, Arbitrary c)
      => Arbitrary (KindDistinguished (a :: Bool) b c) where
    arbitrary = genericArbitrary

$(deriveShow1 'KindDistinguishedUnit)
$(deriveShow2 'KindDistinguishedUnit)

$(deriveShow1 'KindDistinguishedBool)
$(deriveShow2 'KindDistinguishedBool)

$(deriveTextShow  'KindDistinguishedUnit)
$(deriveTextShow1 'KindDistinguishedUnit)
$(deriveTextShow2 'KindDistinguishedUnit)

$(deriveTextShow  'KindDistinguishedBool)
$(deriveTextShow1 'KindDistinguishedBool)
$(deriveTextShow2 'KindDistinguishedBool)

-------------------------------------------------------------------------------

class NullaryClass where
    data NullaryData :: *

instance NullaryClass where
    newtype NullaryData = NullaryCon Int
      deriving (Arbitrary, Show, Generic)

$(deriveTextShow 'NullaryCon)
