{-# LANGUAGE CPP #-}

{-|
Module:      Spec.System.Posix.TypesSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
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
spec = parallel . describe "Text.Show.Text.System.Posix.Types" $ do
    prop "Fd instance"      (prop_matchesShow :: Int -> Fd -> Bool)
#if defined(HTYPE_DEV_T)
    prop "CDev instance"    (prop_matchesShow :: Int -> CDev -> Bool)
#endif
#if defined(HTYPE_INO_T)
    prop "CIno instance"    (prop_matchesShow :: Int -> CIno -> Bool)
#endif
#if defined(HTYPE_MODE_T)
    prop "CMode instance"   (prop_matchesShow :: Int -> CMode -> Bool)
#endif
#if defined(HTYPE_OFF_T)
    prop "COff instance"    (prop_matchesShow :: Int -> COff -> Bool)
#endif
#if defined(HTYPE_PID_T)
    prop "CPid instance"    (prop_matchesShow :: Int -> CPid -> Bool)
#endif
#if defined(HTYPE_SSIZE_T)
    prop "CSsize instance"  (prop_matchesShow :: Int -> CSsize -> Bool)
#endif
#if defined(HTYPE_GID_T)
    prop "CGid instance"    (prop_matchesShow :: Int -> CGid -> Bool)
#endif
#if defined(HTYPE_NLINK_T)
    prop "CNlink instance"  (prop_matchesShow :: Int -> CNlink -> Bool)
#endif
#if defined(HTYPE_UID_T)
    prop "CUid instance"    (prop_matchesShow :: Int -> CUid -> Bool)
#endif
#if defined(HTYPE_CC_T)
    prop "CCc instance"     (prop_matchesShow :: Int -> CCc -> Bool)
#endif
#if defined(HTYPE_SPEED_T)
    prop "CSpeed instance"  (prop_matchesShow :: Int -> CSpeed -> Bool)
#endif
#if defined(HTYPE_TCFLAG_T)
    prop "CTcflag instance" (prop_matchesShow :: Int -> CTcflag -> Bool)
#endif
#if defined(HTYPE_RLIM_T)
    prop "CRLim instance"   (prop_matchesShow :: Int -> CRLim -> Bool)
#endif
