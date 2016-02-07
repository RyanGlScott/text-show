{-|
Module:      Spec.Data.DynamicSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Dynamic'.
-}
module Spec.Data.DynamicSpec (main, spec) where

import Data.Dynamic (Dynamic)

import Instances.Data.Dynamic ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Dynamic" $
    prop "TextShow instance" (prop_matchesTextShow :: Int -> Dynamic -> Bool)
