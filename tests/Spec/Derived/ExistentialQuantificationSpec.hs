{-|
Module:      Spec.Derived.ExistentialQuantificationSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for existentially quantified data types.
-}
module Spec.Derived.ExistentialQuantificationSpec (main, spec) where

import Derived.ExistentialQuantification

import Spec.Utils (prop_matchesShow2)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Data types with existentially quantified type variables" $ do
    describe "TyCon Int Int Int Int" $
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyCon Int Int Int Int -> Bool)
    describe "TyFamily Int Int Int Int" $
        prop "Show2 instance" (prop_matchesShow2 :: Int -> TyFamily Int Int Int Int -> Bool)
