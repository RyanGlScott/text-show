{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.StatsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ test for 'GCStats'.
-}
module Spec.GHC.StatsSpec (main, spec) where

import Instances.GHC.Stats ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,5,0)
import GHC.Stats (GCStats)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,5,0)
    describe "Text.Show.Text.GHC.Stats" $
        prop "GCStats instance" (prop_matchesShow :: Int -> GCStats -> Bool)
#else
    pure ()
#endif
