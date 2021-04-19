{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.TypeLits
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.TypeLits" module.
-}
module Instances.GHC.TypeLits () where

import GHC.TypeLits

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), getNonNegative)

instance Arbitrary SomeNat where
    arbitrary = do
        nat <- getNonNegative <$> arbitrary
        case someNatVal nat of
             Just sn -> pure sn
             Nothing -> error "Negative natural number" -- Should never happen

instance Arbitrary SomeSymbol where
    arbitrary = someSymbolVal <$> arbitrary
