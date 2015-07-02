{-|
Module:      Spec.Data.TupleSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for tuple types.
-}
module Spec.Data.TupleSpec (main, spec) where

import Generics.Deriving.Instances ()

import Instances.Data.Tuple ()

import Spec.Utils (prop_matchesShow, prop_matchesShow2,
                   prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "()" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> () -> Bool)
        prop "generic Show"
            (prop_genericShow :: Int -> () -> Bool)
    describe "(Int, Int)" $ do
        prop "Show2 instance"
            (prop_matchesShow2 :: Int -> (Int, Int) -> Bool)
        prop "generic Show"
            (prop_genericShow  :: Int -> (Int, Int) -> Bool)
        prop "generic Show1"
            (prop_genericShow1 :: Int -> (Int, Int) -> Bool)
    describe "(Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow  :: Int -> (Int, Int, Int) -> Bool)
        prop "generic Show"
            (prop_genericShow  :: Int -> (Int, Int, Int) -> Bool)
        prop "generic Show1"
            (prop_genericShow1 :: Int -> (Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow  :: Int -> (Int, Int, Int, Int) -> Bool)
        prop "generic Show"
            (prop_genericShow  :: Int -> (Int, Int, Int, Int) -> Bool)
        prop "generic Show1"
            (prop_genericShow1 :: Int -> (Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow  :: Int -> (Int, Int, Int, Int, Int) -> Bool)
        prop "generic Show"
            (prop_genericShow  :: Int -> (Int, Int, Int, Int, Int) -> Bool)
        prop "generic Show1"
            (prop_genericShow1 :: Int -> (Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow  :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic Show"
            (prop_genericShow  :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic Show1"
            (prop_genericShow1 :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow  :: Int -> (Int, Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic Show"
            (prop_genericShow  :: Int -> (Int, Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic Show1"
            (prop_genericShow1 :: Int -> (Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "Show instance"
            (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
