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

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Generic (ConType)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ConType" $ do
    prop "TextShow instance" (prop_matchesTextShow :: Int -> ConType -> Bool)
    prop "generic TextShow"  (prop_genericTextShow :: Int -> ConType -> Bool)
