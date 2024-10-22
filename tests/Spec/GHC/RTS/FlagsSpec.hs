{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.RTS.Flags
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ConType'.
-}
module Spec.GHC.RTS.FlagsSpec (main, spec) where

import Data.Proxy (Proxy(..))

import GHC.RTS.Flags

import Instances.GHC.RTS.Flags

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "RTSFlags" $
        matchesTextShowSpec (Proxy :: Proxy RTSFlags)
    describe "GCFlags" $
        matchesTextShowSpec (Proxy :: Proxy GCFlags)
    describe "ConcFlags" $
        matchesTextShowSpec (Proxy :: Proxy ConcFlags)
#if MIN_VERSION_base(4,21,0)
    describe "IoManagerFlag" $
        matchesTextShowSpec (Proxy :: Proxy IoManagerFlag)
#elif MIN_VERSION_base(4,15,0)
    describe "IoSubSystem" $
        matchesTextShowSpec (Proxy :: Proxy IoSubSystem)
#endif
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
#if MIN_VERSION_base(4,10,0)
    describe "ParFlags" $
        matchesTextShowSpec (Proxy :: Proxy ParFlags)
#endif
