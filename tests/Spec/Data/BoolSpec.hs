{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.BoolSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Bool'.
-}
module Spec.Data.BoolSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Spec.Utils (matchesTextShowSpec, genericTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Bool" $ do
    let p :: Proxy Bool
        p = Proxy
    matchesTextShowSpec p
    genericTextShowSpec p
