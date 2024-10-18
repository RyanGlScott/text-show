{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Test.QuickCheck.Instances ()

#if MIN_VERSION_base(4,18,0)
import qualified GHC.TypeNats as TN
import Spec.Utils (GArbitrary(..), Some(..))
#endif

instance Arbitrary SomeNat where
    arbitrary = do
        nat <- getNonNegative <$> arbitrary
        case someNatVal nat of
             Just sn -> pure sn
             Nothing -> error "Negative natural number" -- Should never happen

instance Arbitrary SomeSymbol where
    arbitrary = someSymbolVal <$> arbitrary

#if MIN_VERSION_base(4,16,0)
instance Arbitrary SomeChar where
    arbitrary = someCharVal <$> arbitrary
#endif

#if MIN_VERSION_base(4,18,0)
instance GArbitrary SNat where
  garbitrary = do
    n <- arbitrary
    TN.withSomeSNat n (pure . Some)

instance GArbitrary SSymbol where
  garbitrary = do
    s <- arbitrary
    withSomeSSymbol s (pure . Some)

instance GArbitrary SChar where
  garbitrary = do
    c <- arbitrary
    withSomeSChar c (pure . Some)
#endif
