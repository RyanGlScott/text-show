{-|
Module:      Spec.System.ExitSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'ExitCode'.
-}
module Spec.System.ExitSpec (main, spec) where

import Instances.System.Exit ()

import Spec.Utils (prop_matchesTextShow)

import System.Exit (ExitCode)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ExitCode" $
    prop "TextShow instance" (prop_matchesTextShow :: Int -> ExitCode -> Bool)
