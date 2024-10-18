{-|
Module:      Spec.GHC.StaticPtrSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'StaticPtr'.
-}
module Spec.GHC.StaticPtrSpec (main, spec) where

import Data.Proxy (Proxy(..))

import GHC.StaticPtr (StaticPtrInfo)

import Instances.GHC.StaticPtr ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "StaticPtrInfo" $
        matchesTextShowSpec (Proxy :: Proxy StaticPtrInfo)
