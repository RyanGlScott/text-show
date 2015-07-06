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

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Constr" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Constr -> Bool)
    describe "ConstrRep" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ConstrRep -> Bool)
    describe "DataRep" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DataRep -> Bool)
    describe "DataType" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DataType -> Bool)
    describe "Fixity" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Fixity -> Bool)
