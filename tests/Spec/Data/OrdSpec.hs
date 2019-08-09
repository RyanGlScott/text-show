{-|
Module:      Spec.Data.OrdSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Ord" module.
-}
module Spec.Data.OrdSpec (main, spec) where

import Data.Orphans ()
import Data.Proxy.Compat (Proxy(..))

import Generics.Deriving.Instances ()

import GHC.Exts (Down)

import Instances.Data.Ord ()

import Spec.Utils (matchesTextShowSpec, genericTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Ordering" $ do
        let p :: Proxy Ordering
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "Down Int" $
        matchesTextShowSpec (Proxy :: Proxy (Down Int))
