{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.EitherSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Either'.
-}
module Spec.Data.EitherSpec (main, spec) where

import Data.Orphans ()

import Spec.Utils (prop_matchesShow)
#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_genericShow)
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Either" $ do
    prop "Either Int Int instance"     (prop_matchesShow :: Int -> Either Int Int -> Bool)
#if __GLASGOW_HASKELL__ >= 702
    prop "Either Int Int generic show" (prop_genericShow :: Int -> Either Int Int -> Bool)
#endif
