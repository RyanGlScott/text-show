{-|
Module:      Spec.GenericSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ConType'.
-}
module Spec.GenericSpec (main, spec) where

import Instances.Generic ()

import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Generic (ConType)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ConType" $ do
    prop "Show instance" (prop_matchesShow :: Int -> ConType -> Bool)
    prop "generic Show"  (prop_genericShow :: Int -> ConType -> Bool)
