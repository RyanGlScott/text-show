{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.BoolSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Bool'.
-}
module Spec.Data.BoolSpec (main, spec) where

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Bool" $ do
    prop "TextShow instance" (prop_matchesTextShow :: Int -> Bool -> Bool)
    prop "generic TextShow"  (prop_genericTextShow :: Int -> Bool -> Bool)
