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

import Spec.Utils (prop_matchesShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Identity Int" $
    prop "Show1 instance" (prop_matchesShow1 :: Int -> Identity Int -> Bool)
