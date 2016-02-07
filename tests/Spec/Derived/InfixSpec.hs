{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.InfixSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with infix constructors.
-}
module Spec.Derived.InfixSpec (main, spec) where

import Derived.Infix

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConPlain Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> TyConPlain Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> TyConPlain Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> TyConPlain Int Int -> Bool)
    describe "TyConGADT Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> TyConGADT Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> TyConGADT Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> TyConGADT Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyPlain Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> TyFamilyPlain Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> TyFamilyPlain Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> TyFamilyPlain Int Int -> Bool)
    describe "TyFamilyGADT Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> TyFamilyGADT Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> TyFamilyGADT Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> TyFamilyGADT Int Int -> Bool)
#endif
