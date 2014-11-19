{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.System.Posix.Types (
      showbCDev
    , showbCIno
    , showbCMode
    , showbCOffPrec
    , showbCPidPrec
    , showbCSsizePrec
    , showbCGid
    , showbCNlink
    , showbCUid
    , showbCCc
    , showbCSpeed
    , showbCTcflag
    , showbCRLim
    , showbFdPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import System.Posix.Types

import Text.Show.Text.Class (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Foreign.C.Types ()

-- | Convert a 'CDev' to a 'Builder'.
showbCDev :: CDev -> Builder
showbCDev = showb
{-# INLINE showbCDev #-}

-- | Convert a 'CIno' to a 'Builder'.
showbCIno :: CIno -> Builder
showbCIno = showb
{-# INLINE showbCIno #-}

-- | Convert a 'CMode' to a 'Builder'.
showbCMode :: CMode -> Builder
showbCMode = showb
{-# INLINE showbCMode #-}

-- | Convert a 'COff' to a 'Builder' with the given precedence.
showbCOffPrec :: Int -> COff -> Builder
showbCOffPrec = showbPrec
{-# INLINE showbCOffPrec #-}

-- | Convert a 'CPid' to a 'Builder' with the given precedence.
showbCPidPrec :: Int -> CPid -> Builder
showbCPidPrec = showbPrec
{-# INLINE showbCPidPrec #-}

-- | Convert a 'CSsize' to a 'Builder' with the given precedence.
showbCSsizePrec :: Int -> CSsize -> Builder
showbCSsizePrec = showbPrec
{-# INLINE showbCSsizePrec #-}

-- | Convert a 'CGid' to a 'Builder'.
showbCGid :: CGid -> Builder
showbCGid = showb
{-# INLINE showbCGid #-}

-- | Convert a 'CNlink' to a 'Builder'.
showbCNlink :: CNlink -> Builder
showbCNlink = showb
{-# INLINE showbCNlink #-}

-- | Convert a 'CUid' to a 'Builder'.
showbCUid :: CUid -> Builder
showbCUid = showb
{-# INLINE showbCUid #-}

-- | Convert a 'CCc' to a 'Builder'.
showbCCc :: CCc -> Builder
showbCCc = showb
{-# INLINE showbCCc #-}

-- | Convert a 'CSpeed' to a 'Builder'.
showbCSpeed :: CSpeed -> Builder
showbCSpeed = showb
{-# INLINE showbCSpeed #-}

-- | Convert a 'CTcflag' to a 'Builder'.
showbCTcflag :: CTcflag -> Builder
showbCTcflag = showb
{-# INLINE showbCTcflag #-}

-- | Convert a 'CRLim' to a 'Builder'.
showbCRLim :: CRLim -> Builder
showbCRLim = showb
{-# INLINE showbCRLim #-}

-- | Convert an 'Fd' to a 'Builder' with the given precedence.
showbFdPrec :: Int -> Fd -> Builder
showbFdPrec = showbPrec
{-# INLINE showbFdPrec #-}

deriving instance Show CDev
deriving instance Show CIno
deriving instance Show CMode
deriving instance Show COff
deriving instance Show CPid
deriving instance Show CSsize
deriving instance Show CGid
deriving instance Show CNlink
deriving instance Show CUid
deriving instance Show CCc
deriving instance Show CSpeed
deriving instance Show CTcflag
deriving instance Show CRLim
deriving instance Show Fd