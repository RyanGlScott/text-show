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

import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Bool" $ do
    prop "Show instance" (prop_matchesShow :: Int -> Bool -> Bool)
    prop "generic Show"  (prop_genericShow :: Int -> Bool -> Bool)
