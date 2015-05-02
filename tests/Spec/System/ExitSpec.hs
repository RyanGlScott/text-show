{-|
Module:      Spec.System.ExitSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ test for 'ExitCode'.
-}
module Spec.System.ExitSpec (main, spec) where

import Instances.System.Exit ()

import Spec.Utils (prop_matchesShow)

import System.Exit (ExitCode)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.System.Exit" $
    prop "ExitCode instance" (prop_matchesShow :: Int -> ExitCode -> Bool)
