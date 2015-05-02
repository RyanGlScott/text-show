{-|
Module:      Spec.Data.DynamicSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ test for 'Dynamic'.
-}
module Spec.Data.DynamicSpec (main, spec) where

import Data.Dynamic (Dynamic)

import Instances.Data.Dynamic ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Dynamic" $
    prop "Dynamic instance" (prop_matchesShow :: Int -> Dynamic -> Bool)
