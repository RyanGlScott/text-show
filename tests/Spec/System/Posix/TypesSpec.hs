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

import Spec.Utils (prop_matchesTextShow)

import System.Posix.Types

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

#include "HsBaseConfig.h"

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Fd" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Fd -> Bool)
#if defined(HTYPE_DEV_T)
    describe "CDev" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CDev -> Bool)
#endif
#if defined(HTYPE_INO_T)
    describe "CIno" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CIno -> Bool)
#endif
#if defined(HTYPE_MODE_T)
    describe "CMode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CMode -> Bool)
#endif
#if defined(HTYPE_OFF_T)
    describe "COff" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> COff -> Bool)
#endif
#if defined(HTYPE_PID_T)
    describe "CPid" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CPid -> Bool)
#endif
#if defined(HTYPE_SSIZE_T)
    describe "CSsize" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CSsize -> Bool)
#endif
#if defined(HTYPE_GID_T)
    describe "CGid" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CGid -> Bool)
#endif
#if defined(HTYPE_NLINK_T)
    describe "CNlink" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CNlink -> Bool)
#endif
#if defined(HTYPE_UID_T)
    describe "CUid" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUid -> Bool)
#endif
#if defined(HTYPE_CC_T)
    describe "CCc" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CCc -> Bool)
#endif
#if defined(HTYPE_SPEED_T)
    describe "CSpeed" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CSpeed -> Bool)
#endif
#if defined(HTYPE_TCFLAG_T)
    describe "CTcflag" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CTcflag -> Bool)
#endif
#if defined(HTYPE_RLIM_T)
    describe "CRLim" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CRLim -> Bool)
#endif
