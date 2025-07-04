{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.EventSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.Event" module.
-}
module Spec.GHC.EventSpec (main, spec) where

import Instances.GHC.Event ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(mingw32_HOST_OS)
import Data.Proxy (Proxy(..))

import GHC.Event (Event, Lifetime)

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (describe)
#endif


main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(mingw32_HOST_OS)
    describe "Event" $
        matchesTextShowSpec (Proxy :: Proxy Event)
--     describe "FdKey" $
--         matchesTextShowSpec (Proxy :: Proxy FdKey)
    describe "Lifetime" $
        matchesTextShowSpec (Proxy :: Proxy Lifetime)
#else
    pure ()
#endif
