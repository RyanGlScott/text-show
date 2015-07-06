{-|
Module:      Spec.Data.FixedSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Fixed' values.
-}
module Spec.Data.FixedSpec (main, spec) where

import Data.Fixed (Fixed, E0, E1, E2, E3, E6, E9, E12, showFixed)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Data.Fixed (showbFixed)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Fixed E0" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E0 -> Bool)
    describe "Fixed E1" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E1 -> Bool)
    describe "Fixed E2" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E2 -> Bool)
    describe "Fixed E3" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E3 -> Bool)
    describe "Fixed E6" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E6 -> Bool)
    describe "Fixed E9" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E9 -> Bool)
    describe "Fixed E12" $
        prop "TextShow instance"                    (prop_matchesTextShow :: Int -> Fixed E12 -> Bool)
    describe "showbFixed" $
        prop "has the same output as showFixed"     prop_showFixed

-- | Verifies 'showFixed' and 'showbFixed' generate the same output.
prop_showFixed :: Bool -> Fixed E12 -> Bool
prop_showFixed b f = fromString (showFixed b f) == showbFixed b f
