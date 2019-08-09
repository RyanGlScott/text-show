{-# LANGUAGE CPP #-}
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

import Instances.GHC.Stats ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !(MIN_VERSION_base(4,11,0))
import Data.Proxy.Compat (Proxy(..))
import GHC.Stats (GCStats)
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if !(MIN_VERSION_base(4,11,0))
    describe "GCStats" $
        matchesTextShowSpec (Proxy :: Proxy GCStats)
#else
    pure ()
#endif
