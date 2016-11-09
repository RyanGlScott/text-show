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
import GHC.RTS.Flags

import Instances.GHC.RTS.Flags

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,8,0)
    describe "RTSFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> RTSFlags -> Bool)
    describe "GCFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> GCFlags -> Bool)
    describe "ConcFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ConcFlags -> Bool)
    describe "MiscFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> MiscFlags -> Bool)
    describe "DebugFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DebugFlags -> Bool)
    describe "CCFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CCFlags -> Bool)
    describe "ProfFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ProfFlags -> Bool)
    describe "TraceFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TraceFlags -> Bool)
    describe "TickyFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TickyFlags -> Bool)
    describe "GiveGCStats" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> GiveGCStats' -> Bool)
    describe "DoCostCentres" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DoCostCentres' -> Bool)
    describe "DoHeapProfile" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DoHeapProfile' -> Bool)
    describe "DoTrace" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DoTrace' -> Bool)
# if __GLASGOW_HASKELL__ >= 801
    describe "ParFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ParFlags -> Bool)
# endif
#else
    pure ()
#endif
