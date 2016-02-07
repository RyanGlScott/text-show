{-|
Module:      Spec.FunctionsSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for the orphan 'TextShow' instance for functions.
-}
module Spec.FunctionsSpec (main, spec) where

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Functions ()
import TextShow.Functions ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Int -> Int" $
    prop "TextShow instance" (prop_matchesTextShow :: Int -> (Int -> Int) -> Bool)
