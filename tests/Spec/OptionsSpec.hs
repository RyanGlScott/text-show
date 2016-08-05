{-|
Module:      Spec.OptionsSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Options' and related datatypes.
-}
module Spec.OptionsSpec (main, spec) where

import Instances.Options ()

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.TH (Options, GenTextMethods)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Options" $ do
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Options -> Bool)
        prop "generic TextShow"  (prop_genericTextShow :: Int -> Options -> Bool)
    describe "GenTextMethods" $ do
        prop "TextShow instance" (prop_matchesTextShow :: Int -> GenTextMethods -> Bool)
        prop "generic TextShow"  (prop_genericTextShow :: Int -> GenTextMethods -> Bool)
