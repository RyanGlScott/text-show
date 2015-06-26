{-|
Module:      Spec.Data.MaybeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Maybe'.
-}
module Spec.Data.MaybeSpec (main, spec) where

import Data.Orphans ()

import Spec.Utils (prop_matchesShow1, prop_genericShow, prop_genericShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Maybe Int" $ do
    prop "Show1 instance" (prop_matchesShow1 :: Int -> Maybe Int -> Bool)
    prop "generic Show"   (prop_genericShow  :: Int -> Maybe Int -> Bool)
    prop "generic Show1"  (prop_genericShow1 :: Int -> Maybe Int -> Bool)
