{-|
Module:      Spec.Data.RatioSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Ratio'.
-}
module Spec.Data.RatioSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Data.Ratio (Ratio)

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Ratio Int" $ do
    matchesTextShowSpec (Proxy :: Proxy (Ratio Int))
