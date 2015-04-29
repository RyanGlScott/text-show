{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
#if !(MIN_VERSION_base(4,5,0))
{-# LANGUAGE MagicHash                  #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Posix.Types
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for Haskell equivalents of POSIX data types.
Note that these functions are only available if the operating system supports them,
so some OSes (e.g., Windows) will not be able to use all of the functions in this
module.

/Since: 0.3/
-}
#include "HsBaseConfig.h"

module Text.Show.Text.System.Posix.Types (
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

import Prelude hiding (Show)

import System.Posix.Types

import Text.Show.Text.Classes (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Foreign.C.Types ()

#if !(MIN_VERSION_base(4,5,0))
import Data.Int
import Data.Word

import GHC.Prim (unsafeCoerce#)

# include "HsBaseConfig.h"
# include "inline.h"
#endif

#if defined(HTYPE_DEV_T)
-- | Convert a 'CDev' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCDev :: CDev -> Builder
# if MIN_VERSION_base(4,5,0)
showbCDev = showb
{-# INLINE showbCDev #-}
# else
showbCDev = unsafeCoerce# (showb :: HTYPE_DEV_T -> Builder)
# endif
#endif

#if defined(HTYPE_INO_T)
-- | Convert a 'CIno' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCIno :: CIno -> Builder
# if MIN_VERSION_base(4,5,0)
showbCIno = showb
{-# INLINE showbCIno #-}
# else
showbCIno = unsafeCoerce# (showb :: HTYPE_INO_T -> Builder)
# endif
#endif

#if defined(HTYPE_MODE_T)
-- | Convert a 'CMode' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCMode :: CMode -> Builder
# if MIN_VERSION_base(4,5,0)
showbCMode = showb
{-# INLINE showbCMode #-}
# else
showbCMode = unsafeCoerce# (showb :: HTYPE_MODE_T -> Builder)
# endif
#endif

#if defined(HTYPE_OFF_T)
-- | Convert a 'COff' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbCOffPrec :: Int -> COff -> Builder
# if MIN_VERSION_base(4,5,0)
showbCOffPrec = showbPrec
{-# INLINE showbCOffPrec #-}
# else
showbCOffPrec = unsafeCoerce# (showbPrec :: Int -> HTYPE_OFF_T -> Builder)
# endif
#endif

#if defined(HTYPE_PID_T)
-- | Convert a 'CPid' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbCPidPrec :: Int -> CPid -> Builder
# if MIN_VERSION_base(4,5,0)
showbCPidPrec = showbPrec
{-# INLINE showbCPidPrec #-}
# else
showbCPidPrec = unsafeCoerce# (showbPrec :: Int -> HTYPE_PID_T -> Builder)
# endif
#endif

#if defined(HTYPE_SSIZE_T)
-- | Convert a 'CSsize' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbCSsizePrec :: Int -> CSsize -> Builder
# if MIN_VERSION_base(4,5,0)
showbCSsizePrec = showbPrec
{-# INLINE showbCSsizePrec #-}
# else
showbCSsizePrec = unsafeCoerce# (showbPrec :: Int -> HTYPE_SSIZE_T -> Builder)
# endif
#endif

#if defined(HTYPE_GID_T)
-- | Convert a 'CGid' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCGid :: CGid -> Builder
# if MIN_VERSION_base(4,5,0)
showbCGid = showb
{-# INLINE showbCGid #-}
# else
showbCGid = unsafeCoerce# (showb :: HTYPE_GID_T -> Builder)
# endif
#endif

#if defined(HTYPE_NLINK_T)
-- | Convert a 'CNlink' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCNlink :: CNlink -> Builder
# if MIN_VERSION_base(4,5,0)
showbCNlink = showb
{-# INLINE showbCNlink #-}
# else
showbCNlink = unsafeCoerce# (showb :: HTYPE_NLINK_T -> Builder)
# endif
#endif

#if defined(HTYPE_UID_T)
-- | Convert a 'CUid' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCUid :: CUid -> Builder
# if MIN_VERSION_base(4,5,0)
showbCUid = showb
{-# INLINE showbCUid #-}
# else
showbCUid = unsafeCoerce# (showb :: HTYPE_UID_T -> Builder)
# endif
#endif

#if defined(HTYPE_CC_T)
-- | Convert a 'CCc' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCCc :: CCc -> Builder
# if MIN_VERSION_base(4,5,0)
showbCCc = showb
{-# INLINE showbCCc #-}
# else
showbCCc = unsafeCoerce# (showb :: HTYPE_CC_T -> Builder)
# endif
#endif

#if defined(HTYPE_SPEED_T)
-- | Convert a 'CSpeed' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCSpeed :: CSpeed -> Builder
# if MIN_VERSION_base(4,5,0)
showbCSpeed = showb
{-# INLINE showbCSpeed #-}
# else
showbCSpeed = unsafeCoerce# (showb :: HTYPE_SPEED_T -> Builder)
# endif
#endif

#if defined(HTYPE_TCFLAG_T)
-- | Convert a 'CTcflag' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCTcflag :: CTcflag -> Builder
# if MIN_VERSION_base(4,5,0)
showbCTcflag = showb
{-# INLINE showbCTcflag #-}
# else
showbCTcflag = unsafeCoerce# (showb :: HTYPE_TCFLAG_T -> Builder)
# endif
#endif

#if defined(HTYPE_RLIM_T)
-- | Convert a 'CRLim' to a 'Builder'.
-- 
-- /Since: 0.3/
showbCRLim :: CRLim -> Builder
# if MIN_VERSION_base(4,5,0)
showbCRLim = showb
{-# INLINE showbCRLim #-}
# else
showbCRLim = unsafeCoerce# (showb :: HTYPE_RLIM_T -> Builder)
# endif
#endif

-- | Convert an 'Fd' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbFdPrec :: Int -> Fd -> Builder
showbFdPrec = showbPrec
{-# INLINE showbFdPrec #-}

#if MIN_VERSION_base(4,5,0)
# if defined(HTYPE_DEV_T)
deriving instance Show CDev
# endif

# if defined(HTYPE_INO_T)
deriving instance Show CIno
# endif

# if defined(HTYPE_MODE_T)
deriving instance Show CMode
# endif

# if defined(HTYPE_OFF_T)
deriving instance Show COff
# endif

# if defined(HTYPE_PID_T)
deriving instance Show CPid
# endif

# if defined(HTYPE_SSIZE_T)
deriving instance Show CSsize
# endif

# if defined(HTYPE_GID_T)
deriving instance Show CGid
# endif

# if defined(HTYPE_NLINK_T)
deriving instance Show CNlink
# endif

# if defined(HTYPE_UID_T)
deriving instance Show CUid
# endif

# if defined(HTYPE_CC_T)
deriving instance Show CCc
# endif

# if defined(HTYPE_SPEED_T)
deriving instance Show CSpeed
# endif

# if defined(HTYPE_TCFLAG_T)
deriving instance Show CTcflag
# endif

# if defined(HTYPE_RLIM_T)
deriving instance Show CRLim
# endif
#else
# if defined(HTYPE_DEV_T)
instance Show CDev where
    showb = showbCDev
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_INO_T)
instance Show CIno where
    showb = showbCIno
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_MODE_T)
instance Show CMode where
    showb = showbCMode
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_OFF_T)
instance Show COff where
    showbPrec = showbCOffPrec
    INLINE_INST_FUN(showbPrec)
# endif

# if defined(HTYPE_PID_T)
instance Show CPid where
    showbPrec = showbCPidPrec
    INLINE_INST_FUN(showbPrec)
# endif

# if defined(HTYPE_SSIZE_T)
instance Show CSsize where
    showbPrec = showbCSsizePrec
    INLINE_INST_FUN(showbPrec)
# endif

# if defined(HTYPE_GID_T)
instance Show CGid where
    showb = showbCGid
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_NLINK_T)
instance Show CNlink where
    showb = showbCNlink
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_UID_T)
instance Show CUid where
    showb = showbCUid
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_CC_T)
instance Show CCc where
    showb = showbCCc
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_SPEED_T)
instance Show CSpeed where
    showb = showbCSpeed
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_TCFLAG_T)
instance Show CTcflag where
    showb = showbCTcflag
    INLINE_INST_FUN(showb)
# endif

# if defined(HTYPE_RLIM_T)
instance Show CRLim where
    showb = showbCRLim
    INLINE_INST_FUN(showb)
# endif
#endif

deriving instance Show Fd
