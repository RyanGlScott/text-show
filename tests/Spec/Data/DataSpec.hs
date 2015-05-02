{-|
Module:      Spec.Data.DataSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
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
spec = parallel . describe "Text.Show.Text.Data.Data" $ do
    prop "Constr instance"    (prop_matchesShow :: Int -> Constr -> Bool)
    prop "ConstrRep instance" (prop_matchesShow :: Int -> ConstrRep -> Bool)
    prop "DataRep instance"   (prop_matchesShow :: Int -> DataRep -> Bool)
    prop "DataType instance"  (prop_matchesShow :: Int -> DataType -> Bool)
    prop "Fixity instance"    (prop_matchesShow :: Int -> Fixity -> Bool)
