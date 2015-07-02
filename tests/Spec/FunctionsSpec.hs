{-|
Module:      Spec.FunctionsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for the orphan 'Show' instance for functions.
-}
module Spec.FunctionsSpec (main, spec) where

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Functions ()
import Text.Show.Text.Functions ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Int -> Int" $
    prop "Show instance" (prop_matchesShow :: Int -> (Int -> Int) -> Bool)
