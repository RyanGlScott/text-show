{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.RTS.Flags
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ConType'.
-}
module Spec.GHC.RTS.FlagsSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,8,0)
import Data.Proxy (Proxy(..))
import GHC.RTS.Flags
import Instances.GHC.RTS.Flags
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,8,0)
    describe "RTSFlags" $
        matchesTextShowSpec (Proxy :: Proxy RTSFlags)
    describe "GCFlags" $
        matchesTextShowSpec (Proxy :: Proxy GCFlags)
    describe "ConcFlags" $
        matchesTextShowSpec (Proxy :: Proxy ConcFlags)
    describe "MiscFlags" $
        matchesTextShowSpec (Proxy :: Proxy MiscFlags)
    describe "DebugFlags" $
        matchesTextShowSpec (Proxy :: Proxy DebugFlags)
    describe "CCFlags" $
        matchesTextShowSpec (Proxy :: Proxy CCFlags)
    describe "ProfFlags" $
        matchesTextShowSpec (Proxy :: Proxy ProfFlags)
    describe "TraceFlags" $
        matchesTextShowSpec (Proxy :: Proxy TraceFlags)
    describe "TickyFlags" $
        matchesTextShowSpec (Proxy :: Proxy TickyFlags)
    describe "GiveGCStats" $
        matchesTextShowSpec (Proxy :: Proxy GiveGCStats')
    describe "DoCostCentres" $
        matchesTextShowSpec (Proxy :: Proxy DoCostCentres')
    describe "DoHeapProfile" $
        matchesTextShowSpec (Proxy :: Proxy DoHeapProfile')
    describe "DoTrace" $
        matchesTextShowSpec (Proxy :: Proxy DoTrace')
# if __GLASGOW_HASKELL__ >= 801
    describe "ParFlags" $
        matchesTextShowSpec (Proxy :: Proxy ParFlags)
# endif
#else
    pure ()
#endif
