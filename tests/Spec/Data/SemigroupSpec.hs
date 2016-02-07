{-|
Module:      Spec.Data.SemigroupSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Semigroup" module.
-}
module Spec.Data.SemigroupSpec (main, spec) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import Instances.Data.Semigroup ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Min Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Min Int -> Bool)
    describe "Max Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Max Int -> Bool)
    describe "First Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> First Int -> Bool)
    describe "Last Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Last Int -> Bool)
    describe "WrappedMonoid ()" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> WrappedMonoid () -> Bool)
    describe "Option Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Option Int -> Bool)
    describe "Arg Int Char" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Arg Int Char -> Bool)
