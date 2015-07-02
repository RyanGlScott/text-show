{-# LANGUAGE CPP #-}

{-|
Module:      Spec.System.Posix.TypesSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "System.Posix.Types" module.
-}
module Spec.System.Posix.TypesSpec (main, spec) where

import Instances.System.Posix.Types ()

import Spec.Utils (prop_matchesShow)

import System.Posix.Types

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

#include "HsBaseConfig.h"

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Fd" $
        prop "Show instance" (prop_matchesShow :: Int -> Fd -> Bool)
#if defined(HTYPE_DEV_T)
    describe "CDev" $
        prop "Show instance" (prop_matchesShow :: Int -> CDev -> Bool)
#endif
#if defined(HTYPE_INO_T)
    describe "CIno" $
        prop "Show instance" (prop_matchesShow :: Int -> CIno -> Bool)
#endif
#if defined(HTYPE_MODE_T)
    describe "CMode" $
        prop "Show instance" (prop_matchesShow :: Int -> CMode -> Bool)
#endif
#if defined(HTYPE_OFF_T)
    describe "COff" $
        prop "Show instance" (prop_matchesShow :: Int -> COff -> Bool)
#endif
#if defined(HTYPE_PID_T)
    describe "CPid" $
        prop "Show instance" (prop_matchesShow :: Int -> CPid -> Bool)
#endif
#if defined(HTYPE_SSIZE_T)
    describe "CSsize" $
        prop "Show instance" (prop_matchesShow :: Int -> CSsize -> Bool)
#endif
#if defined(HTYPE_GID_T)
    describe "CGid" $
        prop "Show instance" (prop_matchesShow :: Int -> CGid -> Bool)
#endif
#if defined(HTYPE_NLINK_T)
    describe "CNlink" $
        prop "Show instance" (prop_matchesShow :: Int -> CNlink -> Bool)
#endif
#if defined(HTYPE_UID_T)
    describe "CUid" $
        prop "Show instance" (prop_matchesShow :: Int -> CUid -> Bool)
#endif
#if defined(HTYPE_CC_T)
    describe "CCc" $
        prop "Show instance" (prop_matchesShow :: Int -> CCc -> Bool)
#endif
#if defined(HTYPE_SPEED_T)
    describe "CSpeed" $
        prop "Show instance" (prop_matchesShow :: Int -> CSpeed -> Bool)
#endif
#if defined(HTYPE_TCFLAG_T)
    describe "CTcflag" $
        prop "Show instance" (prop_matchesShow :: Int -> CTcflag -> Bool)
#endif
#if defined(HTYPE_RLIM_T)
    describe "CRLim" $
        prop "Show instance" (prop_matchesShow :: Int -> CRLim -> Bool)
#endif
