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

import Spec.Utils (prop_matchesShow)
#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_genericShow)
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Monoid" $ do
    prop "All instance"                   (prop_matchesShow :: Int -> All -> Bool)
    prop "Any instance"                   (prop_matchesShow :: Int -> Any -> Bool)
    prop "Dual Int instance"              (prop_matchesShow :: Int -> Dual Int -> Bool)
    prop "First (Maybe Int) instance"     (prop_matchesShow :: Int -> First (Maybe Int) -> Bool)
    prop "Last (Maybe Int) instance"      (prop_matchesShow :: Int -> Last (Maybe Int) -> Bool)
    prop "Product Int instance"           (prop_matchesShow :: Int -> Product Int -> Bool)
    prop "Sum Int instance"               (prop_matchesShow :: Int -> Sum Int -> Bool)
#if MIN_VERSION_base(4,8,0)
    prop "Alt Maybe Int instance"         (prop_matchesShow :: Int -> Alt Maybe Int -> Bool)
#endif
#if __GLASGOW_HASKELL__ >= 702
    prop "All generic show"               (prop_genericShow :: Int -> All -> Bool)
    prop "Any generic show"               (prop_genericShow :: Int -> Any -> Bool)
    prop "Dual Int generic show"          (prop_genericShow :: Int -> Dual Int -> Bool)
    prop "First (Maybe Int) generic show" (prop_genericShow :: Int -> First (Maybe Int) -> Bool)
    prop "Last (Maybe Int) generic show"  (prop_genericShow :: Int -> Last (Maybe Int) -> Bool)
    prop "Product Int generic show"       (prop_genericShow :: Int -> Product Int -> Bool)
    prop "Sum Int generic show"           (prop_genericShow :: Int -> Sum Int -> Bool)
#endif
#if MIN_VERSION_base(4,8,0)
    prop "Alt Maybe Int generic show"     (prop_genericShow :: Int -> Alt Maybe Int -> Bool)
#endif
