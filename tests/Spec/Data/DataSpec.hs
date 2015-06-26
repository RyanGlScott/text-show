{-|
Module:      Spec.Data.DataSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Data" module.
-}
module Spec.Data.DataSpec (main, spec) where

import Data.Data (Constr, ConstrRep, DataRep, DataType, Fixity)

import Instances.Data.Data ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Constr" $
        prop "Show instance" (prop_matchesShow :: Int -> Constr -> Bool)
    describe "ConstrRep" $
        prop "Show instance" (prop_matchesShow :: Int -> ConstrRep -> Bool)
    describe "DataRep" $
        prop "Show instance" (prop_matchesShow :: Int -> DataRep -> Bool)
    describe "DataType" $
        prop "Show instance" (prop_matchesShow :: Int -> DataType -> Bool)
    describe "Fixity" $
        prop "Show instance" (prop_matchesShow :: Int -> Fixity -> Bool)
