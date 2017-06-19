{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-|
Module:      Spec.GHC.StatsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'GCStats'.
-}
module Spec.GHC.StatsSpec (main, spec) where

import Data.Proxy (Proxy(..))

import GHC.Stats (GCStats)

import Instances.GHC.Stats ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "GCStats" $
        matchesTextShowSpec (Proxy :: Proxy GCStats)
