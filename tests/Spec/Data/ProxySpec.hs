{-|
Module:      Spec.Data.ProxySpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Proxy'.
-}
module Spec.Data.ProxySpec (main, spec) where

import Data.Proxy (Proxy)

import Generics.Deriving.Base ()

import Instances.Data.Proxy ()

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Proxy Int" $ do
    prop "TextShow instance" (prop_matchesTextShow :: Int -> Proxy Int -> Bool)
    prop "generic TextShow"  (prop_genericTextShow :: Int -> Proxy Int -> Bool)
