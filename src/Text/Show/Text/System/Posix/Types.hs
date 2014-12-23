{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
#if !(MIN_VERSION_base(4,5,0))
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Posix.Types
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for Haskell equivalents of POSIX data types.
-}
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

import Text.Show.Text.Classes (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Foreign.C.Types ()

#if !(MIN_VERSION_base(4,5,0))
import GHC.Prim (unsafeCoerce#)

import Text.Show.Text.Data.Integral ( showbInt32Prec
                                    , showbInt64Prec
                                    , showbWord8
                                    , showbWord32
                                    , showbWord64
                                    )

# include "inline.h"
#endif

-- | Convert a 'CDev' to a 'Builder'.
showbCDev :: CDev -> Builder
#if MIN_VERSION_base(4,5,0)
showbCDev = showb
{-# INLINE showbCDev #-}
#else
showbCDev c = showbWord64 $ unsafeCoerce# c
#endif

-- | Convert a 'CIno' to a 'Builder'.
showbCIno :: CIno -> Builder
#if MIN_VERSION_base(4,5,0)
showbCIno = showb
{-# INLINE showbCIno #-}
#else
showbCIno c = showbWord64 $ unsafeCoerce# c
#endif

-- | Convert a 'CMode' to a 'Builder'.
showbCMode :: CMode -> Builder
#if MIN_VERSION_base(4,5,0)
showbCMode = showb
{-# INLINE showbCMode #-}
#else
showbCMode c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'COff' to a 'Builder' with the given precedence.
showbCOffPrec :: Int -> COff -> Builder
#if MIN_VERSION_base(4,5,0)
showbCOffPrec = showbPrec
{-# INLINE showbCOffPrec #-}
#else
showbCOffPrec p c = showbInt64Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CPid' to a 'Builder' with the given precedence.
showbCPidPrec :: Int -> CPid -> Builder
#if MIN_VERSION_base(4,5,0)
showbCPidPrec = showbPrec
{-# INLINE showbCPidPrec #-}
#else
showbCPidPrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CSsize' to a 'Builder' with the given precedence.
showbCSsizePrec :: Int -> CSsize -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSsizePrec = showbPrec
{-# INLINE showbCSsizePrec #-}
#else
showbCSsizePrec p c = showbInt32Prec p $ unsafeCoerce# c
#endif

-- | Convert a 'CGid' to a 'Builder'.
showbCGid :: CGid -> Builder
#if MIN_VERSION_base(4,5,0)
showbCGid = showb
{-# INLINE showbCGid #-}
#else
showbCGid c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CNlink' to a 'Builder'.
showbCNlink :: CNlink -> Builder
#if MIN_VERSION_base(4,5,0)
showbCNlink = showb
{-# INLINE showbCNlink #-}
#else
showbCNlink c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CUid' to a 'Builder'.
showbCUid :: CUid -> Builder
#if MIN_VERSION_base(4,5,0)
showbCUid = showb
{-# INLINE showbCUid #-}
#else
showbCUid c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CCc' to a 'Builder'.
showbCCc :: CCc -> Builder
#if MIN_VERSION_base(4,5,0)
showbCCc = showb
{-# INLINE showbCCc #-}
#else
showbCCc c = showbWord8 $ unsafeCoerce# c
#endif

-- | Convert a 'CSpeed' to a 'Builder'.
showbCSpeed :: CSpeed -> Builder
#if MIN_VERSION_base(4,5,0)
showbCSpeed = showb
{-# INLINE showbCSpeed #-}
#else
showbCSpeed c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CTcflag' to a 'Builder'.
showbCTcflag :: CTcflag -> Builder
#if MIN_VERSION_base(4,5,0)
showbCTcflag = showb
{-# INLINE showbCTcflag #-}
#else
showbCTcflag c = showbWord32 $ unsafeCoerce# c
#endif

-- | Convert a 'CRLim' to a 'Builder'.
showbCRLim :: CRLim -> Builder
#if MIN_VERSION_base(4,5,0)
showbCRLim = showb
{-# INLINE showbCRLim #-}
#else
showbCRLim c = showbWord64 $ unsafeCoerce# c
#endif

-- | Convert an 'Fd' to a 'Builder' with the given precedence.
showbFdPrec :: Int -> Fd -> Builder
showbFdPrec = showbPrec
{-# INLINE showbFdPrec #-}

#if MIN_VERSION_base(4,5,0)
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
#else
instance Show CDev where
    showb = showbCDev
    INLINE(showb)

instance Show CIno where
    showb = showbCIno
    INLINE(showb)

instance Show CMode where
    showb = showbCMode
    INLINE(showb)

instance Show COff where
    showbPrec = showbCOffPrec
    INLINE(showbPrec)

instance Show CPid where
    showbPrec = showbCPidPrec
    INLINE(showbPrec)

instance Show CSsize where
    showbPrec = showbCSsizePrec
    INLINE(showbPrec)

instance Show CGid where
    showb = showbCGid
    INLINE(showb)

instance Show CNlink where
    showb = showbCNlink
    INLINE(showb)

instance Show CUid where
    showb = showbCUid
    INLINE(showb)

instance Show CCc where
    showb = showbCCc
    INLINE(showb)

instance Show CSpeed where
    showb = showbCSpeed
    INLINE(showb)

instance Show CTcflag where
    showb = showbCTcflag
    INLINE(showb)

instance Show CRLim where
    showb = showbCRLim
    INLINE(showb)
#endif

deriving instance Show Fd