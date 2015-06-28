{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.PolyKindsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with poly-kinded type variables.
-}
module Spec.Derived.PolyKindsSpec (main, spec) where

import Derived.PolyKinds

import Spec.Utils (prop_matchesShow2, prop_genericShow, prop_genericShow1)
#if MIN_VERSION_template_haskell(2,7,0)
import Spec.Utils (prop_genericShow')
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConCompose Either Either Either Maybe Maybe Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyConCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic Show"   (prop_genericShow  :: Int -> TyConCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> TyConCompose Either Either Either Maybe Maybe Int Int -> Bool)
    describe "TyConProxy Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyConProxy Int Int -> Bool)
        prop "generic Show"   (prop_genericShow  :: Int -> TyConProxy Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> TyConProxy Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyCompose Either Either Either Maybe Maybe Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyFamilyCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic Show"   (prop_genericShow' :: Int -> TyFamilyCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> TyFamilyCompose Either Either Either Maybe Maybe Int Int -> Bool)
    describe "TyFamilyProxy Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyFamilyProxy Int Int -> Bool)
        prop "generic Show"   (prop_genericShow' :: Int -> TyFamilyProxy Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> TyFamilyProxy Int Int -> Bool)
#endif
