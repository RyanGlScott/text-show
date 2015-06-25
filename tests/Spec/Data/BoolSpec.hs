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

import Spec.Utils (prop_matchesShow)
#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_genericShow)
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Bool" $ do
    prop "Bool instance"     (prop_matchesShow :: Int -> Bool -> Bool)
#if __GLASGOW_HASKELL__ >= 702
    prop "Bool generic show" (prop_genericShow :: Int -> Bool -> Bool)
#endif
