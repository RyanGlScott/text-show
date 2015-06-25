{-|
Module:      Spec.FromStringTextShowSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'FromStringShow' and 'FromTextShow'.
-}
module Spec.FromStringTextShowSpec (main, spec) where

import Instances.FromStringTextShow ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (FromStringShow(..), FromTextShow(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text" $ do
    describe "FromStringShow Int" $
        prop "Show instance" (prop_matchesShow :: Int -> FromStringShow Int -> Bool)
    describe "FromStringShow String" $
        prop "Show instance" (prop_matchesShow :: Int -> FromStringShow String -> Bool)
    describe "FromTextShow Int" $
        prop "Show instance" (prop_matchesShow :: Int -> FromTextShow Int -> Bool)
    describe "FromTextShow String" $
        prop "Show instance" (prop_matchesShow :: Int -> FromTextShow String -> Bool)
