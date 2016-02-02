{-|
Module:      Spec.Data.Functor.SumSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Sum'.
-}
module Spec.Data.Functor.SumSpec (main, spec) where

import Control.Monad.Trans.Instances ()

import Data.Functor.Sum (Sum)

import Instances.Data.Functor.Sum ()

import Spec.Utils (prop_matchesTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Sum Maybe Maybe Int" $
    prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> Sum Maybe Maybe Int -> Bool)

