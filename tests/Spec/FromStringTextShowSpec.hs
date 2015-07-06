{-|
Module:      Spec.FromStringTextShowSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'FromStringShow' and 'FromTextShow'.
-}
module Spec.FromStringTextShowSpec (main, spec) where

import Instances.FromStringTextShow ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (FromStringShow(..), FromTextShow(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "FromStringShow Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> FromStringShow Int -> Bool)
    describe "FromStringShow String" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> FromStringShow String -> Bool)
    describe "FromTextShow Int" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> FromTextShow Int -> Bool)
    describe "FromTextShow String" $
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> FromTextShow String -> Bool)
