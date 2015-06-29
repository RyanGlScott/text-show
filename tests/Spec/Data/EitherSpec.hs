{-|
Module:      Spec.Data.EitherSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Either'.
-}
module Spec.Data.EitherSpec (main, spec) where

import Generics.Deriving.Instances ()

import Spec.Utils (prop_matchesShow2, prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Either Int Int" $ do
    prop "Show2 instance" (prop_matchesShow2 :: Int -> Either Int Int -> Bool)
    prop "generic Show"   (prop_genericShow  :: Int -> Either Int Int -> Bool)
    prop "generic Show1"  (prop_genericShow1 :: Int -> Either Int Int -> Bool)
