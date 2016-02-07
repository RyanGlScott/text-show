{-|
Module:      Spec.Data.Functor.ComposeSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Compose'.
-}
module Spec.Data.Functor.ComposeSpec (main, spec) where

import Control.Monad.Trans.Instances ()

import Data.Functor.Compose (Compose)

import Instances.Data.Functor.Compose ()

import Spec.Utils (prop_matchesTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Compose Maybe Maybe Int" $
    prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> Compose Maybe Maybe Int -> Bool)

