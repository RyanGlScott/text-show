{-|
Module:      Spec.Data.Functor.Identity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Identity'.
-}
module Spec.Data.Functor.IdentitySpec (main, spec) where

import Data.Functor.Classes ()
import Data.Functor.Identity (Identity)

import Instances.Data.Functor.Identity ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Functor.Identity" $ 
    prop "Identity Int instance"     (prop_matchesShow :: Int -> Identity Int -> Bool)
