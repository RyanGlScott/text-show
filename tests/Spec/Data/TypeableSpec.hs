{-|
Module:      Spec.Data.TypeableSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Typeable" module.
-}
module Spec.Data.TypeableSpec (main, spec) where

import Data.Typeable (TyCon, TypeRep)

import Instances.Data.Typeable ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TypeRep" $
        prop "Show instance" (prop_matchesShow :: Int -> TypeRep -> Bool)
    describe "TyCon" $
        prop "Show instance" (prop_matchesShow :: Int -> TyCon -> Bool)
