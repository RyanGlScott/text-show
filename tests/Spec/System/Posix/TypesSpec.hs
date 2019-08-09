{-# LANGUAGE CPP #-}

{-|
Module:      Spec.System.Posix.TypesSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "System.Posix.Types" module.
-}
module Spec.System.Posix.TypesSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Instances.System.Posix.Types ()
import Spec.Utils (matchesTextShowSpec)
import System.Posix.Types
import Test.Hspec (Spec, describe, hspec, parallel)

#include "HsBaseConfig.h"

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Fd" $
        matchesTextShowSpec (Proxy :: Proxy Fd)
#if defined(HTYPE_DEV_T)
    describe "CDev" $
        matchesTextShowSpec (Proxy :: Proxy CDev)
#endif
#if defined(HTYPE_INO_T)
    describe "CIno" $
        matchesTextShowSpec (Proxy :: Proxy CIno)
#endif
#if defined(HTYPE_MODE_T)
    describe "CMode" $
        matchesTextShowSpec (Proxy :: Proxy CMode)
#endif
#if defined(HTYPE_OFF_T)
    describe "COff" $
        matchesTextShowSpec (Proxy :: Proxy COff)
#endif
#if defined(HTYPE_PID_T)
    describe "CPid" $
        matchesTextShowSpec (Proxy :: Proxy CPid)
#endif
#if defined(HTYPE_SSIZE_T)
    describe "CSsize" $
        matchesTextShowSpec (Proxy :: Proxy CSsize)
#endif
#if defined(HTYPE_GID_T)
    describe "CGid" $
        matchesTextShowSpec (Proxy :: Proxy CGid)
#endif
#if defined(HTYPE_NLINK_T)
    describe "CNlink" $
        matchesTextShowSpec (Proxy :: Proxy CNlink)
#endif
#if defined(HTYPE_UID_T)
    describe "CUid" $
        matchesTextShowSpec (Proxy :: Proxy CUid)
#endif
#if defined(HTYPE_CC_T)
    describe "CCc" $
        matchesTextShowSpec (Proxy :: Proxy CCc)
#endif
#if defined(HTYPE_SPEED_T)
    describe "CSpeed" $
        matchesTextShowSpec (Proxy :: Proxy CSpeed)
#endif
#if defined(HTYPE_TCFLAG_T)
    describe "CTcflag" $
        matchesTextShowSpec (Proxy :: Proxy CTcflag)
#endif
#if defined(HTYPE_RLIM_T)
    describe "CRLim" $
        matchesTextShowSpec (Proxy :: Proxy CRLim)
#endif

#if MIN_VERSION_base(4,10,0)
# if defined(HTYPE_BLKSIZE_T)
    describe "CBlkSize" $
        matchesTextShowSpec (Proxy :: Proxy CBlkSize)
# endif
# if defined(HTYPE_BLKCNT_T)
    describe "CBlkCnt" $
        matchesTextShowSpec (Proxy :: Proxy CBlkCnt)
# endif
# if defined(HTYPE_CLOCKID_T)
    describe "CClockId" $
        matchesTextShowSpec (Proxy :: Proxy CClockId)
# endif
# if defined(HTYPE_FSBLKCNT_T)
    describe "CFsBlkCnt" $
        matchesTextShowSpec (Proxy :: Proxy CFsBlkCnt)
# endif
# if defined(HTYPE_FSFILCNT_T)
    describe "CFsFilCnt" $
        matchesTextShowSpec (Proxy :: Proxy CFsFilCnt)
# endif
# if defined(HTYPE_ID_T)
    describe "CId" $
        matchesTextShowSpec (Proxy :: Proxy CId)
# endif
# if defined(HTYPE_KEY_T)
    describe "CKey" $
        matchesTextShowSpec (Proxy :: Proxy CKey)
# endif
# if defined(HTYPE_TIMER_T)
    describe "CTimer" $
        matchesTextShowSpec (Proxy :: Proxy CTimer)
# endif
#endif
