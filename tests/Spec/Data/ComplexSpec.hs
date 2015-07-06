{-|
Module:      Spec.Data.ComplexSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Complex'.
-}
module Spec.Data.ComplexSpec (main, spec) where

import Data.Complex (Complex)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Complex Double" $
    prop "TextShow instance" (prop_matchesTextShow :: Int -> Complex Double -> Bool)
