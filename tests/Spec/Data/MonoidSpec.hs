{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.MonoidSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Monoid" module.
-}
module Spec.Data.MonoidSpec (main, spec) where

import Data.Monoid

import Generics.Deriving.Instances ()

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "All" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> All -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> All -> Bool)
    describe "Any" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Any -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Any -> Bool)
    describe "Dual Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Dual Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Dual Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Dual Int -> Bool)
    describe "First Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> First Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> First Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> First Int -> Bool)
    describe "Last Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Last Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Last Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Last Int -> Bool)
    describe "Product Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Product Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Product Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Product Int -> Bool)
    describe "Sum Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Sum Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Sum Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Sum Int -> Bool)
#if MIN_VERSION_base(4,8,0)
    describe "Alt Maybe Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Alt Maybe Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Alt Maybe Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> Alt Maybe Int -> Bool)
#endif
