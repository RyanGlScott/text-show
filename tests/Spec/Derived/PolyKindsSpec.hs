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

import Spec.Utils (prop_matchesTextShow1, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConCompose Either Either Either Maybe Maybe Int Int" $ do
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyConCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> TyConCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> TyConCompose Either Either Either Maybe Maybe Int Int -> Bool)
    describe "TyConProxy Int Int" $ do
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyConProxy Int Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> TyConProxy Int Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> TyConProxy Int Int -> Bool)
    describe "TyConReallyHighKinds (,,,,) Int Int Int Int Int" $ do
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyConReallyHighKinds (,,,,) Int Int Int Int Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> TyConReallyHighKinds (,,,,) Int Int Int Int Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> TyConReallyHighKinds (,,,,) Int Int Int Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyCompose Either Either Either Maybe Maybe Int Int" $ do
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyFamilyCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> TyFamilyCompose Either Either Either Maybe Maybe Int Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> TyFamilyCompose Either Either Either Maybe Maybe Int Int -> Bool)
    describe "TyFamilyProxy Int Int" $ do
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyFamilyProxy Int Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> TyFamilyProxy Int Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> TyFamilyProxy Int Int -> Bool)
    describe "TyFamilyReallyHighKinds (,,,,) Int Int Int Int Int" $ do
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyFamilyReallyHighKinds (,,,,) Int Int Int Int Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> TyFamilyReallyHighKinds (,,,,) Int Int Int Int Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> TyFamilyReallyHighKinds (,,,,) Int Int Int Int Int -> Bool)
#endif
