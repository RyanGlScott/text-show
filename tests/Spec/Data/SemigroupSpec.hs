{-|
Module:      Spec.Data.SemigroupSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Semigroup" module.
-}
module Spec.Data.SemigroupSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Arg)

import Instances.Data.Semigroup ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Min Int" $
        matchesTextShowSpec  (Proxy :: Proxy (Min Int))
    describe "Max Int" $
        matchesTextShowSpec  (Proxy :: Proxy (Max Int))
    describe "First Int" $
        matchesTextShowSpec  (Proxy :: Proxy (First Int))
    describe "Last Int" $
        matchesTextShowSpec  (Proxy :: Proxy (Last Int))
    describe "WrappedMonoid ()" $
        matchesTextShowSpec  (Proxy :: Proxy (WrappedMonoid ()))
    describe "Arg Int Char" $
        matchesTextShowSpec  (Proxy :: Proxy (Arg Int Char))
