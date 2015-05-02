{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.MaybeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for 'Maybe'.
-}
module Spec.Data.MaybeSpec (main, spec) where

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
spec = parallel . describe "Text.Show.Text.Data.Maybe" $ do
        prop "Maybe Int instance"     (prop_matchesShow :: Int -> Maybe Int -> Bool)
#if __GLASGOW_HASKELL__ >= 702
        prop "Maybe Int generic show" (prop_genericShow :: Int -> Maybe Int -> Bool)
#endif
