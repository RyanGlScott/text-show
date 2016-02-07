{-|
Module:      Spec.Data.OrdSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Ord" module.
-}
module Spec.Data.OrdSpec (main, spec) where

import Data.Orphans ()

import Generics.Deriving.Instances ()

import GHC.Exts (Down)

import Instances.Data.Ord ()

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Ordering" $ do
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Ordering -> Bool)
        prop "generic TextShow"  (prop_genericTextShow :: Int -> Ordering -> Bool)
    describe "Down Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Down Int -> Bool)
