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

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Data.Char (asciiTabB)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Char" $
        prop "TextShow instance"   (prop_matchesTextShow :: Int -> Char -> Bool)
    describe "GeneralCategory" $
        prop "TextShow instance"   (prop_matchesTextShow :: Int -> GeneralCategory -> Bool)
    describe "asciiTabB" $
        it "equals asciiTab" $     map fromString asciiTab `shouldBe` elems asciiTabB
