{-# LANGUAGE CPP       #-}

#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE DataKinds #-}
#endif

{-|
Module:      Spec.GHC.TypeLitsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.TypeLits" module.
-}
module Spec.GHC.TypeLitsSpec (main, spec) where

import Instances.GHC.TypeLits ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,6,0)
import GHC.TypeLits

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,7,0)
    describe "SomeNat" $
        prop "Show instance" (prop_matchesShow :: Int -> SomeNat -> Bool)
    describe "SomeSymbol" $
        prop "Show instance" (prop_matchesShow :: Int -> SomeSymbol -> Bool)
#elif MIN_VERSION_base(4,6,0)
    describe "IsEven 0" $
        prop "Show instance" (prop_matchesShow :: Int -> IsEven 0 -> Bool)
    describe "IsEven 1" $
        prop "Show instance" (prop_matchesShow :: Int -> IsEven 1 -> Bool)
    describe "IsEven 2" $
        prop "Show instance" (prop_matchesShow :: Int -> IsEven 2 -> Bool)
    describe "IsZero 0" $
        prop "Show instance" (prop_matchesShow :: Int -> IsZero 0 -> Bool)
    describe "IsZero 1" $
        prop "Show instance" (prop_matchesShow :: Int -> IsZero 1 -> Bool)
    describe "Sing 0" $
        prop "Show instance" (prop_matchesShow :: Int -> Sing 0 -> Bool)
    describe "Sing \"a\"" $
        prop "Show instance" (prop_matchesShow :: Int -> Sing "a" -> Bool)
#else
    pure ()
#endif
