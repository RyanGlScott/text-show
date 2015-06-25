{-|
Module:      Spec.Control.ApplicativeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Applicative" module.
-}
module Spec.Control.ApplicativeSpec (main, spec) where

import Control.Applicative (Const, ZipList)

import Data.Orphans ()

import Instances.Control.Applicative ()

import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Control.Applicative" $ do
    prop "Const Int Int instance"   (prop_matchesShow :: Int -> Const Int Int -> Bool)
    prop "ZipList Int instance"     (prop_matchesShow :: Int -> ZipList Int -> Bool)
    prop "ZipList Int generic show" (prop_genericShow :: Int -> ZipList Int -> Bool)
