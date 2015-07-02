{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Generic
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'ConType'.
-}
module Instances.Generic () where

import Instances.Data.Text ()

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

import Text.Show.Text.Generic (ConType(..))

instance Arbitrary ConType where
    arbitrary = oneof [pure Rec, pure Tup, pure Pref, Inf <$> arbitrary]
