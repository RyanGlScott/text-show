{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.RTS.Flags
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for 'ConType'.
-}
module Spec.GHC.RTS.FlagsSpec (main, spec) where

import Instances.GHC.RTS.Flags ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,8,0)
import GHC.RTS.Flags

import Spec.Utils (ioProperty)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,8,0)
    describe "Text.Show.Text.GHC.RTS.Flags" $ do
        prop "RTSFlags instance"   prop_showRTSFlags
        prop "GCFlags instance"    prop_showGCFlags
        prop "ConcFlags instance"  (prop_matchesShow :: Int -> ConcFlags -> Bool)
        prop "MiscFlags instance"  (prop_matchesShow :: Int -> MiscFlags -> Bool)
        prop "DebugFlags instance" (prop_matchesShow :: Int -> DebugFlags -> Bool)
        prop "CCFlags instance"    prop_showCCFlags
        prop "ProfFlags instance"  prop_showProfFlags
        prop "TraceFlags instance" prop_showTraceFlags
        prop "TickyFlags instance" (prop_matchesShow :: Int -> TickyFlags -> Bool)
#else
    pure ()
#endif

#if MIN_VERSION_base(4,8,0)
-- | Verifies that the 'Show' instance for 'RTSFlags' is accurate.
prop_showRTSFlags :: Int -> Property
prop_showRTSFlags p = ioProperty $ do
    rtsflags <- getRTSFlags
    pure $ prop_matchesShow p rtsflags

-- | Verifies that the 'Show' instance for 'GCFlags' is accurate.
prop_showGCFlags :: Int -> Property
prop_showGCFlags p = ioProperty $ do
    gcflags <- getGCFlags
    pure $ prop_matchesShow p gcflags

-- | Verifies that the 'Show' instance for 'CCFlags' is accurate.
prop_showCCFlags :: Int -> Property
prop_showCCFlags p = ioProperty $ do
    ccflags <- getCCFlags
    pure $ prop_matchesShow p ccflags

-- | Verifies that the 'Show' instance for 'ProfFlags' is accurate.
prop_showProfFlags :: Int -> Property
prop_showProfFlags p = ioProperty $ do
    profflags <- getProfFlags
    pure $ prop_matchesShow p profflags

-- | Verifies that the 'Show' instance for 'TraceFlags' is accurate.
prop_showTraceFlags :: Int -> Property
prop_showTraceFlags p = ioProperty $ do
    traceflags <- getTraceFlags
    pure $ prop_matchesShow p traceflags
#endif
