{-|
Module:      Spec.Control.ApplicativeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Applicative" module.
-}
module Spec.Control.ApplicativeSpec (main, spec) where

import Control.Applicative (Const, ZipList)

import Data.Orphans ()

import Instances.Control.Applicative ()

import Spec.Utils (prop_matchesShow, prop_matchesShow2,
                   prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Const Int Int" $
        prop "Show2 instance" (prop_matchesShow2 :: Int -> Const Int Int -> Bool)
    describe "ZipList Int" $ do
        prop "Show instance"  (prop_matchesShow  :: Int -> ZipList Int -> Bool)
        prop "generic Show"   (prop_genericShow  :: Int -> ZipList Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> ZipList Int -> Bool)
