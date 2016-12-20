{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.System.Posix.Types
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "System.Posix.Types" module.
-}
module Instances.System.Posix.Types () where

import Instances.Foreign.C.Types ()
import System.Posix.Types
import Test.QuickCheck (Arbitrary(..))

#if !(MIN_VERSION_base(4,5,0))
import Data.Word
import Test.QuickCheck (Gen, arbitrarySizedBoundedIntegral)
import Unsafe.Coerce (unsafeCoerce)
#endif

#include "HsBaseConfig.h"

#if MIN_VERSION_base(4,5,0)
# if defined(HTYPE_DEV_T)
deriving instance Arbitrary CDev
# endif
# if defined(HTYPE_INO_T)
deriving instance Arbitrary CIno
# endif
# if defined(HTYPE_MODE_T)
deriving instance Arbitrary CMode
# endif
# if defined(HTYPE_OFF_T)
deriving instance Arbitrary COff
# endif
# if defined(HTYPE_PID_T)
deriving instance Arbitrary CPid
# endif
# if defined(HTYPE_SSIZE_T)
deriving instance Arbitrary CSsize
# endif
# if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
# endif
# if defined(HTYPE_NLINK_T)
deriving instance Arbitrary CNlink
# endif
# if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
# endif
# if defined(HTYPE_CC_T)
deriving instance Arbitrary CCc
# endif
# if defined(HTYPE_SPEED_T)
deriving instance Arbitrary CSpeed
# endif
# if defined(HTYPE_TCFLAG_T)
deriving instance Arbitrary CTcflag
# endif
# if defined(HTYPE_RLIM_T)
deriving instance Arbitrary CRLim
# endif
#else
# if defined(HTYPE_DEV_T)
instance Arbitrary CDev where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_DEV_T)
# endif
# if defined(HTYPE_INO_T)
instance Arbitrary CIno where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_MODE_T)
instance Arbitrary CMode where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_OFF_T)
instance Arbitrary COff where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_PID_T)
instance Arbitrary CPid where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_SSIZE_T)
instance Arbitrary CSsize where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_GID_T)
instance Arbitrary CGid where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_NLINK_T)
instance Arbitrary CNlink where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_UID_T)
instance Arbitrary CUid where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_CC_T)
instance Arbitrary CCc where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_CC_T)
# endif
# if defined(HTYPE_SPEED_T)
instance Arbitrary CSpeed where
    arbitrary = unsafeCoerce (arbitrary :: Gen HTYPE_SPEED_T)
# endif
# if defined(HTYPE_TCFLAG_T)
instance Arbitrary CTcflag where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
# if defined(HTYPE_RLIM_T)
instance Arbitrary CRLim where
    arbitrary = arbitrarySizedBoundedIntegral
# endif
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
#endif

deriving instance Arbitrary Fd
