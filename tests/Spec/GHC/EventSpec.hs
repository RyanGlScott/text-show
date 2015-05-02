{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.EventSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the "GHC.Event" module.
-}
module Spec.GHC.EventSpec (main, spec) where

import Instances.GHC.Event ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !(defined(__GHCJS__)) && MIN_VERSION_base(4,4,0)
import GHC.Event (Event)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif


main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if !(defined(__GHCJS__)) && MIN_VERSION_base(4,4,0)
    describe "Text.Show.Text.GHC.Event" $ do
        prop "Event instance" (prop_matchesShow :: Int -> Event -> Bool)
--         prop "FdKey instance" (prop_matchesShow :: Int -> FdKey -> Bool)
#else
    pure ()
#endif
