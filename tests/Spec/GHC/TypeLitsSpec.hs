{-# LANGUAGE DataKinds #-}

{-|
Module:      Spec.GHC.TypeLitsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.TypeLits" module.
-}
module Spec.GHC.TypeLitsSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))

import GHC.TypeLits

import Instances.GHC.TypeLits ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "SomeNat" $
        matchesTextShowSpec (Proxy :: Proxy SomeNat)
    describe "SomeSymbol" $
        matchesTextShowSpec (Proxy :: Proxy SomeSymbol)
