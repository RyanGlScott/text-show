{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.OrdSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the "Data.Ord" module.
-}
module Spec.Data.OrdSpec (main, spec) where

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)
#endif
import Data.Orphans ()

import Instances.Data.Ord ()

import Spec.Utils (prop_matchesShow)
#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_genericShow)
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Ord" $ do
    prop "Ordering instance"     (prop_matchesShow :: Int -> Ordering -> Bool)
#if __GLASGOW_HASKELL__ >= 702
    prop "Ordering generic show" (prop_genericShow :: Int -> Ordering -> Bool)
#endif
#if MIN_VERSION_base(4,6,0)
    prop "Down Int instance"     (prop_matchesShow :: Int -> Down Int -> Bool)
#endif
