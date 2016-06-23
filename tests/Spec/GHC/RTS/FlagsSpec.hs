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

import Instances.GHC.RTS.Flags ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,8,0)
import GHC.RTS.Flags

import Spec.Utils (ioProperty, prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,8,0)
    describe "RTSFlags" $
        prop "TextShow instance" prop_showRTSFlags
    describe "GCFlags" $
        prop "TextShow instance" prop_showGCFlags
    describe "ConcFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ConcFlags -> Bool)
    describe "MiscFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> MiscFlags -> Bool)
    describe "DebugFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DebugFlags -> Bool)
    describe "CCFlags" $
        prop "TextShow instance" prop_showCCFlags
    describe "ProfFlags" $
        prop "TextShow instance" prop_showProfFlags
    describe "TraceFlags" $
        prop "TextShow instance" prop_showTraceFlags
    describe "TickyFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TickyFlags -> Bool)
# if __GLASGOW_HASKELL__ >= 801
    describe "ParFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ParFlags -> Bool)
# endif
#else
    pure ()
#endif

#if MIN_VERSION_base(4,8,0)
-- | Verifies that the 'Show' instance for 'RTSFlags' is accurate.
prop_showRTSFlags :: Int -> Property
prop_showRTSFlags p = ioProperty $ do
    rtsflags <- getRTSFlags
    pure $ prop_matchesTextShow p rtsflags

-- | Verifies that the 'Show' instance for 'GCFlags' is accurate.
prop_showGCFlags :: Int -> Property
prop_showGCFlags p = ioProperty $ do
    gcflags <- getGCFlags
    pure $ prop_matchesTextShow p gcflags

-- | Verifies that the 'Show' instance for 'CCFlags' is accurate.
prop_showCCFlags :: Int -> Property
prop_showCCFlags p = ioProperty $ do
    ccflags <- getCCFlags
    pure $ prop_matchesTextShow p ccflags

-- | Verifies that the 'Show' instance for 'ProfFlags' is accurate.
prop_showProfFlags :: Int -> Property
prop_showProfFlags p = ioProperty $ do
    profflags <- getProfFlags
    pure $ prop_matchesTextShow p profflags

-- | Verifies that the 'Show' instance for 'TraceFlags' is accurate.
prop_showTraceFlags :: Int -> Property
prop_showTraceFlags p = ioProperty $ do
    traceflags <- getTraceFlags
    pure $ prop_matchesTextShow p traceflags
#endif
