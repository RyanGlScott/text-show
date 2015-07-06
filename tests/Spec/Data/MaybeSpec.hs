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

import Spec.Utils (prop_matchesTextShow1, prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Maybe Int" $ do
    prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> Maybe Int -> Bool)
    prop "generic TextShow"   (prop_genericTextShow  :: Int -> Maybe Int -> Bool)
    prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> Maybe Int -> Bool)
