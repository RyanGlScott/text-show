{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.System.Posix.Types
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "System.Posix.Types" module.
-}
module Instances.System.Posix.Types () where

import Instances.Foreign.C.Types ()
import Instances.Foreign.Ptr ()

import System.Posix.Types

import Test.QuickCheck (Arbitrary(..))

#include "HsBaseConfig.h"

#if defined(HTYPE_DEV_T)
deriving instance Arbitrary CDev
#endif

#if defined(HTYPE_INO_T)
deriving instance Arbitrary CIno
#endif

#if defined(HTYPE_MODE_T)
deriving instance Arbitrary CMode
#endif

#if defined(HTYPE_OFF_T)
deriving instance Arbitrary COff
#endif

#if defined(HTYPE_PID_T)
deriving instance Arbitrary CPid
#endif

#if defined(HTYPE_SSIZE_T)
deriving instance Arbitrary CSsize
#endif

#if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
#endif

#if defined(HTYPE_NLINK_T)
deriving instance Arbitrary CNlink
#endif

#if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
#endif

#if defined(HTYPE_CC_T)
deriving instance Arbitrary CCc
#endif

#if defined(HTYPE_SPEED_T)
deriving instance Arbitrary CSpeed
#endif

#if defined(HTYPE_TCFLAG_T)
deriving instance Arbitrary CTcflag
#endif

#if defined(HTYPE_RLIM_T)
deriving instance Arbitrary CRLim
#endif

#if MIN_VERSION_base(4,10,0)
# if defined(HTYPE_BLKSIZE_T)
deriving instance Arbitrary CBlkSize
# endif

# if defined(HTYPE_BLKCNT_T)
deriving instance Arbitrary CBlkCnt
# endif

# if defined(HTYPE_CLOCKID_T)
deriving instance Arbitrary CClockId
# endif

# if defined(HTYPE_FSBLKCNT_T)
deriving instance Arbitrary CFsBlkCnt
# endif

# if defined(HTYPE_FSFILCNT_T)
deriving instance Arbitrary CFsFilCnt
# endif

# if defined(HTYPE_ID_T)
deriving instance Arbitrary CId
# endif

# if defined(HTYPE_KEY_T)
deriving instance Arbitrary CKey
# endif

# if defined(HTYPE_TIMER_T)
deriving instance Arbitrary CTimer
# endif
#endif

deriving instance Arbitrary Fd
