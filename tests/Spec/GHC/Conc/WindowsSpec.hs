{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.Conc.WindowsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'ConsoleEvent'.
-}
module Spec.GHC.Conc.WindowsSpec (main, spec) where

import Instances.GHC.Conc.Windows ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(__GHCJS__) && defined(mingw32_HOST_OS)
import GHC.Conc.Windows (ConsoleEvent)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if !defined(__GHCJS__) && defined(mingw32_HOST_OS)
    describe "Text.Show.Text.GHC.Conc.Windows" $
        prop "ConsoleEvent instance" (prop_matchesShow :: Int -> ConsoleEvent -> Bool)
#else
    pure ()
#endif
