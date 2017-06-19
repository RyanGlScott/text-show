{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.System.Posix.Types
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for Haskell equivalents of POSIX data types.
Note that these are only available if the operating system supports them,
so some OSes (e.g., Windows) will not be able to use all of the instances in this
module.

/Since: 2/
-}
module TextShow.System.Posix.Types () where

import System.Posix.Types

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral   ()
import TextShow.Foreign.C.Types ()
import TextShow.Foreign.Ptr     ()

#include "HsBaseConfig.h"

#if defined(HTYPE_DEV_T)
-- | /Since: 2/
deriving instance TextShow CDev
#endif

#if defined(HTYPE_INO_T)
-- | /Since: 2/
deriving instance TextShow CIno
#endif

#if defined(HTYPE_MODE_T)
-- | /Since: 2/
deriving instance TextShow CMode
#endif

#if defined(HTYPE_OFF_T)
-- | /Since: 2/
deriving instance TextShow COff
#endif

#if defined(HTYPE_PID_T)
-- | /Since: 2/
deriving instance TextShow CPid
#endif

#if defined(HTYPE_SSIZE_T)
-- | /Since: 2/
deriving instance TextShow CSsize
#endif

#if defined(HTYPE_GID_T)
-- | /Since: 2/
deriving instance TextShow CGid
#endif

#if defined(HTYPE_NLINK_T)
-- | /Since: 2/
deriving instance TextShow CNlink
#endif

#if defined(HTYPE_UID_T)
-- | /Since: 2/
deriving instance TextShow CUid
#endif

#if defined(HTYPE_CC_T)
-- | /Since: 2/
deriving instance TextShow CCc
#endif

#if defined(HTYPE_SPEED_T)
-- | /Since: 2/
deriving instance TextShow CSpeed
#endif

#if defined(HTYPE_TCFLAG_T)
-- | /Since: 2/
deriving instance TextShow CTcflag
#endif

#if defined(HTYPE_RLIM_T)
-- | /Since: 2/
deriving instance TextShow CRLim
#endif

#if MIN_VERSION_base(4,10,0)
# if defined(HTYPE_BLKSIZE_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CBlkSize
# endif

# if defined(HTYPE_BLKCNT_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CBlkCnt
# endif

# if defined(HTYPE_CLOCKID_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CClockId
# endif

# if defined(HTYPE_FSBLKCNT_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CFsBlkCnt
# endif

# if defined(HTYPE_FSFILCNT_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CFsFilCnt
# endif

# if defined(HTYPE_ID_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CId
# endif

# if defined(HTYPE_KEY_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CKey
# endif

# if defined(HTYPE_TIMER_T)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CTimer
# endif
#endif

-- | /Since: 2/
deriving instance TextShow Fd
