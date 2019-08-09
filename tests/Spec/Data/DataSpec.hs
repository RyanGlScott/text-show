{-|
Module:      Spec.Data.DataSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Data" module.
-}
module Spec.Data.DataSpec (main, spec) where

import Data.Data (Constr, ConstrRep, DataRep, DataType, Fixity)
import Data.Proxy.Compat (Proxy(..))

import Instances.Data.Data ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Constr" $
        matchesTextShowSpec (Proxy :: Proxy Constr)
    describe "ConstrRep" $
        matchesTextShowSpec (Proxy :: Proxy ConstrRep)
    describe "DataRep" $
        matchesTextShowSpec (Proxy :: Proxy DataRep)
    describe "DataType" $
        matchesTextShowSpec (Proxy :: Proxy DataType)
    describe "Fixity" $
        matchesTextShowSpec (Proxy :: Proxy Fixity)
