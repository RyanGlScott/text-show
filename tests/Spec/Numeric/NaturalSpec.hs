{-|
Module:      Spec.Numeric.NaturalSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Natural'.
-}
module Spec.Numeric.NaturalSpec (main, spec) where

import Instances.Numeric.Natural ()

import Numeric.Natural (Natural)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Natural" $
    prop "Show instance" (prop_matchesShow :: Int -> Natural -> Bool)
