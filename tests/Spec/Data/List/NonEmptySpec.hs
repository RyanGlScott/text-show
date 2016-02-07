{-|
Module:      Spec.Data.List.NonEmptySpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'NonEmpty'.
-}
module Spec.Data.List.NonEmptySpec (main, spec) where

import Data.List.NonEmpty (NonEmpty)

import Instances.Data.List.NonEmpty ()

import Spec.Utils (prop_matchesTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "NonEmpty Int" $
    prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> NonEmpty Int -> Bool)
