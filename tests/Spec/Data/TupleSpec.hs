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

import Spec.Utils (prop_matchesTextShow, prop_matchesTextShow1,
                   prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "()" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> () -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow :: Int -> () -> Bool)
    describe "(Int, Int)" $ do
        prop "TextShow1 instance"
            (prop_matchesTextShow1 :: Int -> (Int, Int) -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow  :: Int -> (Int, Int) -> Bool)
        prop "generic TextShow1"
            (prop_genericTextShow1 :: Int -> (Int, Int) -> Bool)
    describe "(Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow  :: Int -> (Int, Int, Int) -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow  :: Int -> (Int, Int, Int) -> Bool)
        prop "generic TextShow1"
            (prop_genericTextShow1 :: Int -> (Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow  :: Int -> (Int, Int, Int, Int) -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow  :: Int -> (Int, Int, Int, Int) -> Bool)
        prop "generic TextShow1"
            (prop_genericTextShow1 :: Int -> (Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow  :: Int -> (Int, Int, Int, Int, Int) -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow  :: Int -> (Int, Int, Int, Int, Int) -> Bool)
        prop "generic TextShow1"
            (prop_genericTextShow1 :: Int -> (Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow  :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow  :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic TextShow1"
            (prop_genericTextShow1 :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow  :: Int -> (Int, Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic TextShow"
            (prop_genericTextShow  :: Int -> (Int, Int, Int, Int, Int, Int, Int) -> Bool)
        prop "generic TextShow1"
            (prop_genericTextShow1 :: Int -> (Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        prop "TextShow instance"
            (prop_matchesTextShow :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Bool)
