{-|
Module:      Spec.Data.DynamicSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Dynamic'.
-}
module Spec.Data.DynamicSpec (main, spec) where

import Data.Dynamic (Dynamic)
import Data.Proxy.Compat (Proxy(..))

import Instances.Data.Dynamic ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Dynamic" $
    matchesTextShowSpec (Proxy :: Proxy Dynamic)
