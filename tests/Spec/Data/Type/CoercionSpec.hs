{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.Type.CoercionSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Coercion'.
-}
module Spec.Data.Type.CoercionSpec (main, spec) where

import Data.Monoid (All(..))
import Data.Proxy (Proxy(..))
import Data.Type.Coercion (Coercion)

import Instances.Data.Type.Coercion ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "Coercion All Bool" $
        matchesTextShowSpec (Proxy :: Proxy (Coercion All Bool))
