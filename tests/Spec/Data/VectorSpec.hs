{-|
Module:      Spec.Data.VectorSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for vectors.
-}
module Spec.Data.VectorSpec (main, spec) where

import Instances.Data.Text()
import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Data.Text (Text)
import Data.Vector (Vector)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Vector Text" $
        prop "TextShow instance"                       (prop_matchesTextShow :: Int -> Vector Text -> Bool)
    describe "Vector String" $
        prop "TextShow instance"                       (prop_matchesTextShow :: Int -> Vector String -> Bool)
    describe "Vector Int" $
        prop "TextShow instance"                       (prop_matchesTextShow :: Int -> Vector Int -> Bool)
