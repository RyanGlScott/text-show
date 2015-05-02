{-|
Module:      Spec.Data.ListSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for lists.
-}
module Spec.Data.ListSpec (main, spec) where

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show (showListWith)
import Text.Show.Text (fromString, showb)
import Text.Show.Text.Data.List (showbListWith)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.List" $ do
    prop "String instance"      (prop_matchesShow :: Int -> String -> Bool)
    prop "[String] instance"    (prop_matchesShow :: Int -> [String] -> Bool)
    prop "[Int] instance"       (prop_matchesShow :: Int -> [Int] -> Bool)
    prop "showbListWith output" prop_showListWith

-- | Verifies 'showListWith' and 'showbListWith' generate the same output.
prop_showListWith :: String -> Bool
prop_showListWith str = fromString (showListWith shows str "") == showbListWith showb str
