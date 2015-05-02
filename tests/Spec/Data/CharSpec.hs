{-|
Module:      Spec.Data.CharSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the "Data.Char" module.
-}
module Spec.Data.CharSpec (main, spec) where

import Data.Array (elems)
import Data.Char (GeneralCategory)

import GHC.Show (asciiTab)

import Instances.Data.Char ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (fromString)
import Text.Show.Text.Data.Char (asciiTabB)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Char" $ do
    prop "Char instance"            (prop_matchesShow :: Int -> Char -> Bool)
    prop "GeneralCategory instance" (prop_matchesShow :: Int -> GeneralCategory -> Bool)
    it   "asciiTab = asciiTabB"   $ map fromString asciiTab `shouldBe` elems asciiTabB
