{-|
Module:      Spec.Data.CharSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
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
spec = parallel $ do
    describe "Char" $
        prop "Show instance"   (prop_matchesShow :: Int -> Char -> Bool)
    describe "GeneralCategory" $
        prop "Show instance"   (prop_matchesShow :: Int -> GeneralCategory -> Bool)
    describe "asciiTabB" $
        it "equals asciiTab" $ map fromString asciiTab `shouldBe` elems asciiTabB
