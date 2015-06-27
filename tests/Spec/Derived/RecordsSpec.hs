{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.RecordsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with record syntax.
-}
module Spec.Derived.RecordsSpec (main, spec) where

import Derived.Records

import Spec.Utils (prop_matchesShow2, prop_genericShow,
                   prop_genericShow', prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyCon Int Int -> Bool)
        prop "generic Show"   (prop_genericShow  :: Int -> TyCon Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> TyCon Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyFamily Int Int -> Bool)
        prop "generic Show"   (prop_genericShow' :: Int -> TyFamily Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> TyFamily Int Int -> Bool)
#endif
