{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.InfixSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with infix constructors.
-}
module Spec.Derived.InfixSpec (main, spec) where

import Derived.Infix

import Spec.Utils (prop_matchesShow, prop_genericShow, prop_genericShow1)
#if MIN_VERSION_template_haskell(2,7,0)
import Spec.Utils (prop_genericShow')
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConPlain Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> TyConPlain Int Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> TyConPlain Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> TyConPlain Int Int -> Bool)
    describe "TyConGADT Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> TyConGADT Int Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> TyConGADT Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> TyConGADT Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyPlain Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> TyFamilyPlain Int Int -> Bool)
        prop "generic Show"  (prop_genericShow' :: Int -> TyFamilyPlain Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> TyFamilyPlain Int Int -> Bool)
    describe "TyFamilyGADT Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> TyFamilyGADT Int Int -> Bool)
        prop "generic Show"  (prop_genericShow' :: Int -> TyFamilyGADT Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> TyFamilyGADT Int Int -> Bool)
#endif
