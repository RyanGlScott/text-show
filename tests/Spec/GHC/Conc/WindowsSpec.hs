{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.Conc.WindowsSpec
Copyright:   (C) 2014-2017 Ryan Scott
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
import Data.Proxy.Compat (Proxy(..))
import GHC.Conc.Windows (ConsoleEvent)
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if !defined(__GHCJS__) && defined(mingw32_HOST_OS)
    describe "ConsoleEvent" $
        matchesTextShowSpec (Proxy :: Proxy ConsoleEvent)
#else
    pure ()
#endif
