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

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (fromString)
import Text.Show.Text.Data.Fixed (showbFixed)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Fixed" $ do
    prop "Fixed E0 instance"  (prop_matchesShow :: Int -> Fixed E0 -> Bool)
    prop "Fixed E1 instance"  (prop_matchesShow :: Int -> Fixed E1 -> Bool)
    prop "Fixed E2 instance"  (prop_matchesShow :: Int -> Fixed E2 -> Bool)
    prop "Fixed E3 instance"  (prop_matchesShow :: Int -> Fixed E3 -> Bool)
    prop "Fixed E6 instance"  (prop_matchesShow :: Int -> Fixed E6 -> Bool)
    prop "Fixed E9 instance"  (prop_matchesShow :: Int -> Fixed E9 -> Bool)
    prop "Fixed E12 instance" (prop_matchesShow :: Int -> Fixed E12 -> Bool)
    prop "showFixed output"   prop_showFixed

-- | Verifies 'showFixed' and 'showbFixed' generate the same output.
prop_showFixed :: Bool -> Fixed E12 -> Bool
prop_showFixed b f = fromString (showFixed b f) == showbFixed b f
