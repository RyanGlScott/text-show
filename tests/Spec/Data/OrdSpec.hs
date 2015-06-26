{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.OrdSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Ord" module.
-}
module Spec.Data.OrdSpec (main, spec) where

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)
#endif
import Data.Orphans ()

import Instances.Data.Ord ()

import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Ordering" $ do
        prop "Show instance" (prop_matchesShow :: Int -> Ordering -> Bool)
        prop "generic Show"  (prop_genericShow :: Int -> Ordering -> Bool)
#if MIN_VERSION_base(4,6,0)
    describe "Down Int" $ do
        prop "Show instance" (prop_matchesShow :: Int -> Down Int -> Bool)
#endif
