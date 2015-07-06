{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.System.Posix.Types
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for Haskell equivalents of POSIX data types.
Note that these functions are only available if the operating system supports them,
so some OSes (e.g., Windows) will not be able to use all of the functions in this
module.

/Since: 2/
-}
#include "HsBaseConfig.h"

module TextShow.System.Posix.Types (
      showbFdPrec
#if defined(HTYPE_DEV_T)
    , showbCDev
#endif
#if defined(HTYPE_INO_T)
    , showbCIno
#endif
#if defined(HTYPE_MODE_T)
    , showbCMode
#endif
#if defined(HTYPE_OFF_T)
    , showbCOffPrec
#endif
#if defined(HTYPE_PID_T)
    , showbCPidPrec
#endif
#if defined(HTYPE_SSIZE_T)
    , showbCSsizePrec
#endif
#if defined(HTYPE_GID_T)
    , showbCGid
#endif
#if defined(HTYPE_NLINK_T)
    , showbCNlink
#endif
#if defined(HTYPE_UID_T)
    , showbCUid
#endif
#if defined(HTYPE_CC_T)
    , showbCCc
#endif
#if defined(HTYPE_SPEED_T)
    , showbCSpeed
#endif
#if defined(HTYPE_TCFLAG_T)
    , showbCTcflag
#endif
#if defined(HTYPE_RLIM_T)
    , showbCRLim
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import System.Posix.Types

import TextShow.Classes (TextShow(showb, showbPrec))
import TextShow.Data.Integral ()
import TextShow.Foreign.C.Types ()

#if !(MIN_VERSION_base(4,5,0))
import Data.Int
import Data.Word

import Unsafe.Coerce (unsafeCoerce)

# include "HsBaseConfig.h"
# include "inline.h"
#endif

#if defined(HTYPE_DEV_T)
-- | Convert a 'CDev' to a 'Builder'.
--
-- /Since: 2/
showbCDev :: CDev -> Builder
# if MIN_VERSION_base(4,5,0)
showbCDev = showb
{-# INLINE showbCDev #-}
# else
showbCDev = unsafeCoerce (showb :: HTYPE_DEV_T -> Builder)
# endif
#endif

#if defined(HTYPE_INO_T)
-- | Convert a 'CIno' to a 'Builder'.
--
-- /Since: 2/
showbCIno :: CIno -> Builder
# if MIN_VERSION_base(4,5,0)
showbCIno = showb
{-# INLINE showbCIno #-}
# else
showbCIno = unsafeCoerce (showb :: HTYPE_INO_T -> Builder)
# endif
#endif

#if defined(HTYPE_MODE_T)
-- | Convert a 'CMode' to a 'Builder'.
--
-- /Since: 2/
showbCMode :: CMode -> Builder
# if MIN_VERSION_base(4,5,0)
showbCMode = showb
{-# INLINE showbCMode #-}
# else
showbCMode = unsafeCoerce (showb :: HTYPE_MODE_T -> Builder)
# endif
#endif

#if defined(HTYPE_OFF_T)
-- | Convert a 'COff' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCOffPrec :: Int -> COff -> Builder
# if MIN_VERSION_base(4,5,0)
showbCOffPrec = showbPrec
{-# INLINE showbCOffPrec #-}
# else
showbCOffPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_OFF_T -> Builder)
# endif
#endif

#if defined(HTYPE_PID_T)
-- | Convert a 'CPid' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCPidPrec :: Int -> CPid -> Builder
# if MIN_VERSION_base(4,5,0)
showbCPidPrec = showbPrec
{-# INLINE showbCPidPrec #-}
# else
showbCPidPrec = unsafeCoerce (showbPrec :: Int -> HTYPE_PID_T -> Builder)
# endif
#endif

#if defined(HTYPE_SSIZE_T)
-- | Convert a 'CSsize' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCSsizePrec :: Int -> CSsize -> Builder
# if MIN_VERSION_base(4,5,0)
showbCSsizePrec = showbPrec
{-# INLINE showbCSsizePrec #-}
# else
showbCSsizePrec = unsafeCoerce (showbPrec :: Int -> HTYPE_SSIZE_T -> Builder)
# endif
#endif

#if defined(HTYPE_GID_T)
-- | Convert a 'CGid' to a 'Builder'.
--
-- /Since: 2/
showbCGid :: CGid -> Builder
# if MIN_VERSION_base(4,5,0)
showbCGid = showb
{-# INLINE showbCGid #-}
# else
showbCGid = unsafeCoerce (showb :: HTYPE_GID_T -> Builder)
# endif
#endif

#if defined(HTYPE_NLINK_T)
-- | Convert a 'CNlink' to a 'Builder'.
--
-- /Since: 2/
showbCNlink :: CNlink -> Builder
# if MIN_VERSION_base(4,5,0)
showbCNlink = showb
{-# INLINE showbCNlink #-}
# else
showbCNlink = unsafeCoerce (showb :: HTYPE_NLINK_T -> Builder)
# endif
#endif

#if defined(HTYPE_UID_T)
-- | Convert a 'CUid' to a 'Builder'.
--
-- /Since: 2/
showbCUid :: CUid -> Builder
# if MIN_VERSION_base(4,5,0)
showbCUid = showb
{-# INLINE showbCUid #-}
# else
showbCUid = unsafeCoerce (showb :: HTYPE_UID_T -> Builder)
# endif
#endif

#if defined(HTYPE_CC_T)
-- | Convert a 'CCc' to a 'Builder'.
--
-- /Since: 2/
showbCCc :: CCc -> Builder
# if MIN_VERSION_base(4,5,0)
showbCCc = showb
{-# INLINE showbCCc #-}
# else
showbCCc = unsafeCoerce (showb :: HTYPE_CC_T -> Builder)
# endif
#endif

#if defined(HTYPE_SPEED_T)
-- | Convert a 'CSpeed' to a 'Builder'.
--
-- /Since: 2/
showbCSpeed :: CSpeed -> Builder
# if MIN_VERSION_base(4,5,0)
showbCSpeed = showb
{-# INLINE showbCSpeed #-}
# else
showbCSpeed = unsafeCoerce (showb :: HTYPE_SPEED_T -> Builder)
# endif
#endif

#if defined(HTYPE_TCFLAG_T)
-- | Convert a 'CTcflag' to a 'Builder'.
--
-- /Since: 2/
showbCTcflag :: CTcflag -> Builder
# if MIN_VERSION_base(4,5,0)
showbCTcflag = showb
{-# INLINE showbCTcflag #-}
# else
showbCTcflag = unsafeCoerce (showb :: HTYPE_TCFLAG_T -> Builder)
# endif
#endif

#if defined(HTYPE_RLIM_T)
-- | Convert a 'CRLim' to a 'Builder'.
--
-- /Since: 2/
showbCRLim :: CRLim -> Builder
# if MIN_VERSION_base(4,5,0)
showbCRLim = showb
{-# INLINE showbCRLim #-}
# else
showbCRLim = unsafeCoerce (showb :: HTYPE_RLIM_T -> Builder)
# endif
#endif

-- | Convert an 'Fd' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbFdPrec :: Int -> Fd -> Builder
showbFdPrec = showbPrec
{-# INLINE showbFdPrec #-}

#if MIN_VERSION_base(4,5,0)
# if defined(HTYPE_DEV_T)
deriving instance TextShow CDev
# endif

# if defined(HTYPE_INO_T)
deriving instance TextShow CIno
# endif

# if defined(HTYPE_MODE_T)
deriving instance TextShow CMode
# endif

# if defined(HTYPE_OFF_T)
deriving instance TextShow COff
# endif

# if defined(HTYPE_PID_T)
deriving instance TextShow CPid
# endif

# if defined(HTYPE_SSIZE_T)
deriving instance TextShow CSsize
# endif

# if defined(HTYPE_GID_T)
deriving instance TextShow CGid
# endif

# if defined(HTYPE_NLINK_T)
deriving instance TextShow CNlink
# endif

# if defined(HTYPE_UID_T)
deriving instance TextShow CUid
# endif

# if defined(HTYPE_CC_T)
deriving instance TextShow CCc
# endif

# if defined(HTYPE_SPEED_T)
deriving instance TextShow CSpeed
# endif

# if defined(HTYPE_TCFLAG_T)
deriving instance TextShow CTcflag
# endif

# if defined(HTYPE_RLIM_T)
deriving instance TextShow CRLim
# endif
#else
# if defined(HTYPE_DEV_T)
instance TextShow CDev where
    showb = showbCDev
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_INO_T)
instance TextShow CIno where
    showb = showbCIno
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_MODE_T)
instance TextShow CMode where
    showb = showbCMode
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_OFF_T)
instance TextShow COff where
    showbPrec = showbCOffPrec
    INLINE_INST_FUN(showbPrec)
# endif

# if defined(HTYPE_PID_T)
instance TextShow CPid where
    showbPrec = showbCPidPrec
    INLINE_INST_FUN(showbPrec)
# endif

# if defined(HTYPE_SSIZE_T)
instance TextShow CSsize where
    showbPrec = showbCSsizePrec
    INLINE_INST_FUN(showbPrec)
# endif

# if defined(HTYPE_GID_T)
instance TextShow CGid where
    showb = showbCGid
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_NLINK_T)
instance TextShow CNlink where
    showb = showbCNlink
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_UID_T)
instance TextShow CUid where
    showb = showbCUid
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_CC_T)
instance TextShow CCc where
    showb = showbCCc
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_SPEED_T)
instance TextShow CSpeed where
    showb = showbCSpeed
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_TCFLAG_T)
instance TextShow CTcflag where
    showb = showbCTcflag
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_RLIM_T)
instance TextShow CRLim where
    showb = showbCRLim
    INLINE_INST_FUN(showb)
# endif
#endif

deriving instance TextShow Fd
