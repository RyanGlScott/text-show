{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.MonoidSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Monoid" module.
-}
module Spec.Data.MonoidSpec (main, spec) where

import Data.Monoid
import Data.Orphans ()

import Instances.Data.Monoid ()

import Spec.Utils (prop_matchesShow, prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "All" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> All -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> All -> Bool)
    describe "Any" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Any -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Any -> Bool)
    describe "Dual Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Dual Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Dual Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Dual Int -> Bool)
    describe "First Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> First Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> First Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> First Int -> Bool)
    describe "Last Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Last Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Last Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Last Int -> Bool)
    describe "Product Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Product Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Product Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Product Int -> Bool)
    describe "Sum Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Sum Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Sum Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Sum Int -> Bool)
#if MIN_VERSION_base(4,8,0)
    describe "Alt Maybe Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Alt Maybe Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Alt Maybe Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> Alt Maybe Int -> Bool)
#endif
