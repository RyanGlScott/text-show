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

import Spec.Utils (prop_matchesTextShow1, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Either Int Int" $ do
    prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> Either Int Int -> Bool)
    prop "generic TextShow"   (prop_genericTextShow  :: Int -> Either Int Int -> Bool)
    prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> Either Int Int -> Bool)
